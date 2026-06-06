#' Validate the quality of a sales time-series table
#'
#' Runs a battery of data-quality checks **without modifying the data** and
#' returns an S3 object of class `"sales_validation"` describing what was found.
#' The object has a Polish-language [print()] method and a [summary()] method
#' returning a tidy tibble.
#'
#' @param data A sales tibble (e.g. from [load_sales_data()]); must contain the
#'   columns `date`, `store_nbr`, `family`, `sales`, `onpromotion`.
#' @param value_min Lower bound for valid `sales` values (default `0`).
#' @param value_max Upper bound for valid `sales` values (default `Inf`).
#'
#' @return An S3 object of class `"sales_validation"`: a list with the issue
#'   counts and supporting tibbles.
#'
#' @details The following checks are performed:
#'   * missing (`NA`) values per column,
#'   * duplicated `(date, store_nbr, family)` keys,
#'   * `sales` values outside `[value_min, value_max]` and negative values,
#'   * date gaps within each `store_nbr x family` series (frequency consistency),
#'   * dates in the future (relative to [Sys.Date()]),
#'   * `onpromotion` values outside `{0, 1, NA}`.
#'
#' @examples
#' val <- validate_sales_ts(sample_sales)
#' val
#' summary(val)
#'
#' @export
validate_sales_ts <- function(data, value_min = 0, value_max = Inf) {
  check_columns(data, c("date", "store_nbr", "family", "sales", "onpromotion"))
  if (!is.numeric(value_min) || length(value_min) != 1L) {
    cli::cli_abort("Argument {.arg value_min} musi by\u0107 pojedyncz\u0105 liczb\u0105.")
  }
  if (!is.numeric(value_max) || length(value_max) != 1L) {
    cli::cli_abort("Argument {.arg value_max} musi by\u0107 pojedyncz\u0105 liczb\u0105.")
  }

  # Missing values per column.
  na_counts <- data |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "kolumna", values_to = "liczba_NA"
    ) |>
    dplyr::filter(.data$liczba_NA > 0)

  # Duplicated keys.
  dupes <- data |>
    dplyr::count(.data$date, .data$store_nbr, .data$family, name = "n") |>
    dplyr::filter(.data$n > 1L)
  n_duplicates <- sum(dupes$n) - nrow(dupes)

  # Out-of-range and negative sales.
  out_of_range <- sum(data$sales < value_min | data$sales > value_max, na.rm = TRUE)
  n_negative <- sum(data$sales < 0, na.rm = TRUE)

  # Date gaps per series.
  gaps <- detect_date_gaps(data)
  n_series_with_gaps <- dplyr::n_distinct(gaps$store_nbr, gaps$family)
  n_missing_dates <- sum(gaps$missing_days)

  # Future dates.
  n_future <- sum(data$date > Sys.Date(), na.rm = TRUE)

  # Invalid onpromotion.
  promo_ok <- is.na(data$onpromotion) | data$onpromotion %in% c(0L, 1L)
  n_bad_promo <- sum(!promo_ok)

  freq <- detect_frequency(data$date)

  issues <- tibble::tibble(
    sprawdzenie = c(
      "Braki NA (suma)", "Duplikaty klucza", "Poza zakresem",
      "Warto\u015bci ujemne", "Serie z lukami", "Brakuj\u0105ce dni (luki)",
      "Daty z przysz\u0142o\u015bci", "B\u0142\u0119dne onpromotion"
    ),
    liczba = c(
      sum(na_counts$liczba_NA), n_duplicates, out_of_range, n_negative,
      n_series_with_gaps, n_missing_dates, n_future, n_bad_promo
    )
  )

  structure(
    list(
      n_rows = nrow(data),
      frequency = freq,
      value_min = value_min,
      value_max = value_max,
      na_counts = na_counts,
      duplicates = dupes,
      n_duplicates = n_duplicates,
      out_of_range = out_of_range,
      n_negative = n_negative,
      gaps = gaps,
      n_future = n_future,
      n_bad_promo = n_bad_promo,
      issues = issues
    ),
    class = "sales_validation"
  )
}

# Find missing calendar dates within each store x family series.
detect_date_gaps <- function(data) {
  data |>
    dplyr::filter(!is.na(.data$date)) |>
    dplyr::distinct(.data$store_nbr, .data$family, .data$date) |>
    dplyr::group_by(.data$store_nbr, .data$family) |>
    dplyr::summarise(
      missing_days = {
        rng <- range(.data$date)
        expected <- as.numeric(diff(rng)) + 1
        expected - dplyr::n()
      },
      .groups = "drop"
    ) |>
    dplyr::filter(.data$missing_days > 0)
}

#' @export
print.sales_validation <- function(x, ...) {
  cli::cli_h1("Walidacja szeregu sprzeda\u017cy")
  cli::cli_text("Liczba wierszy: {.val {x$n_rows}}; wykryta cz\u0119stotliwo\u015b\u0107: {.val {x$frequency}}.")
  total_issues <- sum(x$issues$liczba)
  if (total_issues == 0) {
    cli::cli_alert_success("Nie wykryto problem\u00f3w jako\u015bci danych.")
  } else {
    cli::cli_alert_warning("Wykryto problemy jako\u015bci danych (szczeg\u00f3\u0142y poni\u017cej).")
  }
  for (i in seq_len(nrow(x$issues))) {
    val <- x$issues$liczba[i]
    label <- x$issues$sprawdzenie[i]
    if (val > 0) {
      cli::cli_alert_warning("{label}: {.val {val}}")
    } else {
      cli::cli_alert_success("{label}: {.val {val}}")
    }
  }
  invisible(x)
}

#' @export
#' @rdname validate_sales_ts
#' @param object A `sales_validation` object.
#' @param ... Unused; for S3 compatibility.
summary.sales_validation <- function(object, ...) {
  object$issues
}
