#' Forecast a single sales series with ARIMA and/or Prophet
#'
#' Fits univariate forecasting models to one sales series (a single
#' `store_nbr x family` series, or the aggregate of everything) and returns tidy
#' forecasts, optionally with a chronological backtest comparing model accuracy.
#'
#' @param data A sales tibble with `date` and `sales` (and, for `series`
#'   filtering, `store_nbr` / `family`).
#' @param series Optional `list(store_nbr = , family = )` selecting one series. If
#'   `NULL`, all stores/families are summed into a single aggregate series.
#' @param models Character vector of models to fit; subset of
#'   `c("arima", "prophet")`.
#' @param horizon Number of future periods to forecast.
#' @param frequency Aggregation frequency: `"day"`, `"week"` or `"month"`.
#' @param backtest Logical; if `TRUE`, run a chronological train/test split and
#'   report accuracy metrics.
#' @param test_size Fraction of the series held out for the backtest (default
#'   `0.2`).
#'
#' @return An S3 object of class `"prognosis"`: a list with `forecast` (tidy
#'   tibble of `date`, `model`, `forecast`, `lo95`, `hi95`), `history` (the
#'   prepared series), `backtest` (accuracy tibble or `NULL`), `best_model` and
#'   metadata. Has a [plot()] method.
#'
#' @details **This function is strictly univariate.** Modelling all
#'   54 x 33 = 1782 series of the full panel would be prohibitively expensive
#'   without parallelism, so forecast one series at a time -- e.g. drive it across
#'   a chosen subset with [sales_ts_logic()] and a `filters`/`group_by` argument.
#'   Prophet is optional: if the \pkg{prophet} package is not installed it is
#'   skipped with a warning instead of erroring.
#'
#' @examples
#' fc <- create_prognosis(
#'   sample_sales,
#'   series = list(store_nbr = 1, family = "BEVERAGES"),
#'   models = "arima",
#'   horizon = 14,
#'   backtest = TRUE
#' )
#' fc$backtest
#' \dontrun{
#' plot(fc)
#' }
#'
#' @export
create_prognosis <- function(data,
                             series = NULL,
                             models = c("arima", "prophet"),
                             horizon = 30,
                             frequency = c("day", "week", "month"),
                             backtest = TRUE,
                             test_size = 0.2) {
  check_columns(data, c("date", "sales"))
  models <- intersect(models, c("arima", "prophet"))
  if (length(models) == 0L) {
    cli::cli_abort("Argument {.arg models} musi zawiera\u0107 'arima' i/lub 'prophet'.")
  }
  frequency <- rlang::arg_match(frequency)
  check_count(horizon, "horizon")
  if (!is.numeric(test_size) || length(test_size) != 1L || test_size <= 0 || test_size >= 1) {
    cli::cli_abort("Argument {.arg test_size} musi by\u0107 liczb\u0105 z przedzia\u0142u (0, 1).")
  }

  # Prophet availability: drop it gracefully if missing.
  if ("prophet" %in% models && !requireNamespace("prophet", quietly = TRUE)) {
    cli::cli_warn("Pakiet {.pkg prophet} nie jest zainstalowany - pomijam model Prophet.")
    models <- setdiff(models, "prophet")
  }
  if (length(models) == 0L) {
    cli::cli_abort("Brak dost\u0119pnych modeli do dopasowania (Prophet niedost\u0119pny).")
  }

  ser <- prepare_series(data, series, frequency)
  step <- switch(frequency, day = "day", week = "week", month = "month")
  if (nrow(ser) < 4L) {
    cli::cli_abort(c(
      "Seria jest za kr\u00f3tka do prognozowania.",
      "x" = "Liczba obserwacji: {.val {nrow(ser)}}.",
      "i" = "Wybierz inn\u0105 cz\u0119stotliwo\u015b\u0107 albo d\u0142u\u017cszy zakres dat."
    ))
  }
  # Seasonal period for the ARIMA `ts` object; fall back to non-seasonal (1) when
  # the series is shorter than two full cycles so short series still forecast.
  season <- switch(frequency, day = 7L, week = 52L, month = 12L)
  freq_num <- if (nrow(ser) >= 2L * season) season else 1L

  future_dates <- seq(max(ser$date), by = step, length.out = horizon + 1L)[-1L]

  forecasts <- purrr::map(models, function(m) {
    fit_forecast(m, ser, horizon, freq_num, future_dates)
  }) |>
    dplyr::bind_rows()

  bt <- NULL
  best_model <- NULL
  if (backtest) {
    bt <- run_backtest(ser, models, freq_num, test_size)
    if (!is.null(bt) && nrow(bt) > 0) {
      best_model <- bt$model[which.min(bt$RMSE)]
    }
  }

  structure(
    list(
      forecast = forecasts,
      history = ser,
      backtest = bt,
      best_model = best_model,
      models = models,
      frequency = frequency,
      horizon = horizon,
      series = series
    ),
    class = "prognosis"
  )
}

# Filter (or aggregate) to a single series, interpolate NA, aggregate to freq.
prepare_series <- function(data, series, frequency, call = rlang::caller_env()) {
  if (!is.null(series)) {
    if (!is.list(series)) {
      cli::cli_abort("Argument {.arg series} musi by\u0107 list\u0105, np. list(store_nbr = 3, family = 'BEVERAGES').",
        call = call
      )
    }
    if (!is.null(series$store_nbr)) {
      check_columns(data, "store_nbr", call = call)
      data <- dplyr::filter(data, .data$store_nbr %in% series$store_nbr)
    }
    if (!is.null(series$family)) {
      check_columns(data, "family", call = call)
      data <- dplyr::filter(data, .data$family %in% series$family)
    }
    if (nrow(data) == 0L) {
      cli::cli_abort("Wybrana seria jest pusta (sprawd\u017a {.arg series}).", call = call)
    }
  }

  ser <- data |>
    dplyr::mutate(date = lubridate::floor_date(.data$date, unit = frequency)) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(y = sum(.data$sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$date)

  # Regularise the calendar and interpolate any remaining gaps.
  full <- tibble::tibble(date = seq(min(ser$date), max(ser$date), by = frequency))
  full |>
    dplyr::left_join(ser, by = "date") |>
    dplyr::mutate(y = as.numeric(zoo::na.approx(.data$y, na.rm = FALSE, rule = 2)))
}

# Fit one model to the full series and return a tidy forecast tibble.
fit_forecast <- function(model, ser, horizon, freq_num, future_dates) {
  if (model == "arima") {
    y <- stats::ts(ser$y, frequency = freq_num)
    fit <- forecast::auto.arima(y)
    fc <- forecast::forecast(fit, h = horizon, level = 95)
    tibble::tibble(
      date = future_dates,
      model = "arima",
      forecast = as.numeric(fc$mean),
      lo95 = as.numeric(fc$lower[, 1]),
      hi95 = as.numeric(fc$upper[, 1])
    )
  } else {
    df <- data.frame(ds = ser$date, y = ser$y)
    m <- suppressMessages(prophet::prophet(df, weekly.seasonality = TRUE))
    fut <- prophet::make_future_dataframe(m, periods = horizon, freq = step_for(future_dates))
    pred <- stats::predict(m, fut)
    pred <- utils::tail(pred, horizon)
    tibble::tibble(
      date = as.Date(pred$ds),
      model = "prophet",
      forecast = pred$yhat,
      lo95 = pred$yhat_lower,
      hi95 = pred$yhat_upper
    )
  }
}

# Infer the prophet `freq` string from the spacing of the future dates.
step_for <- function(future_dates) {
  if (length(future_dates) < 2L) {
    return("day")
  }
  gap <- as.numeric(diff(future_dates)[1])
  if (gap <= 3) "day" else if (gap <= 16) "week" else "month"
}

# Chronological backtest for the requested models.
run_backtest <- function(ser, models, freq_num, test_size) {
  n <- nrow(ser)
  n_train <- floor((1 - test_size) * n)
  if (n_train < freq_num + 2L || n_train >= n) {
    cli::cli_warn("Seria zbyt kr\u00f3tka na backtest - pomijam ocen\u0119 dok\u0142adno\u015bci.")
    return(NULL)
  }
  train <- ser[seq_len(n_train), ]
  test <- ser[(n_train + 1L):n, ]
  h <- nrow(test)

  purrr::map(models, function(m) {
    pred <- backtest_predict(m, train, test, freq_num, h)
    acc <- accuracy_metrics(test$y, pred)
    tibble::tibble(model = m, RMSE = acc[["RMSE"]], MAE = acc[["MAE"]], MAPE = acc[["MAPE"]])
  }) |>
    dplyr::bind_rows()
}

backtest_predict <- function(model, train, test, freq_num, h) {
  if (model == "arima") {
    y <- stats::ts(train$y, frequency = freq_num)
    fit <- forecast::auto.arima(y)
    as.numeric(forecast::forecast(fit, h = h)$mean)
  } else {
    df <- data.frame(ds = train$date, y = train$y)
    m <- suppressMessages(prophet::prophet(df, weekly.seasonality = TRUE))
    fut <- data.frame(ds = test$date)
    stats::predict(m, fut)$yhat
  }
}

accuracy_metrics <- function(actual, pred) {
  err <- actual - pred
  nz <- actual != 0
  c(
    RMSE = sqrt(mean(err^2, na.rm = TRUE)),
    MAE = mean(abs(err), na.rm = TRUE),
    MAPE = if (any(nz)) mean(abs(err[nz] / actual[nz]), na.rm = TRUE) * 100 else NA_real_
  )
}

#' @export
print.prognosis <- function(x, ...) {
  cli::cli_h1("Prognoza sprzeda\u017cy")
  cli::cli_text(
    "Modele: {.val {x$models}}; cz\u0119stotliwo\u015b\u0107: {.val {x$frequency}}; ",
    "horyzont: {.val {x$horizon}} okres(\u00f3w)."
  )
  if (!is.null(x$best_model)) {
    cli::cli_alert_success("Lepszy model wg RMSE (backtest): {.val {x$best_model}}.")
    print(x$backtest)
  }
  invisible(x)
}

#' @export
#' @rdname create_prognosis
#' @param x A `prognosis` object.
#' @param ... Unused; for S3 compatibility.
plot.prognosis <- function(x, ...) {
  hist <- x$history
  fc <- x$forecast

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = hist,
      mapping = ggplot2::aes(x = .data$date, y = .data$y),
      colour = "grey30"
    ) +
    ggplot2::geom_ribbon(
      data = fc,
      mapping = ggplot2::aes(
        x = .data$date, ymin = .data$lo95, ymax = .data$hi95, fill = .data$model
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = fc,
      mapping = ggplot2::aes(x = .data$date, y = .data$forecast, colour = .data$model),
      linewidth = 0.9
    ) +
    ggplot2::labs(
      title = "Historia i prognoza sprzeda\u017cy",
      subtitle = "Wst\u0119ga: 95% przedzia\u0142 ufno\u015bci",
      x = "Data", y = "Sprzeda\u017c", colour = "Model", fill = "Model"
    ) +
    ggplot2::theme_minimal()
}
