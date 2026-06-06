#' Build a Polish-language management summary of sales
#'
#' Produces an S3 object of class `"mgmt_summary"` bundling the key facts a retail
#' manager cares about: best/worst stores, fastest-growing and fastest-declining
#' categories, year-over-year change, promotion effectiveness, revenue
#' concentration, sudden-drop alerts and (when available) holiday effects. Its
#' [print()] method renders a narrative in Polish together with supporting tables.
#'
#' @param data A sales tibble enriched with store metadata (`city`, `state`,
#'   `type`); typically the output of [load_sales_data()] with `stores_path` set,
#'   or `sample_sales` joined with `sample_stores`.
#' @param period_days Length (in days) of the "current" period; the immediately
#'   preceding window of the same length is used for comparisons, and the same
#'   window one year earlier for the year-over-year figures. Defaults to `30`.
#'
#' @return An S3 object of class `"mgmt_summary"` (a list of the computed tables
#'   and scalars), with a Polish [print()] method.
#'
#' @details Comparisons are anchored on the latest date present in `data`. The
#'   "alerts" section flags any `store_nbr x family` series whose weekly sales fell
#'   by more than 30% week-over-week within the last two weeks. Revenue
#'   concentration is reported as the top-5 store share plus a Herfindahl-Hirschman
#'   index over all stores.
#'
#' @examples
#' joined <- dplyr::left_join(sample_sales, sample_stores, by = "store_nbr")
#' summary_obj <- create_management_summary(joined, period_days = 30)
#' summary_obj
#'
#' @export
create_management_summary <- function(data, period_days = 30) {
  check_columns(
    data,
    c("date", "store_nbr", "family", "sales", "onpromotion", "city", "state", "type")
  )
  check_count(period_days, "period_days")

  max_date <- max(data$date, na.rm = TRUE)
  p_start <- max_date - period_days + 1
  prev_end <- p_start - 1
  prev_start <- prev_end - period_days + 1
  yoy_start <- p_start - 365
  yoy_end <- max_date - 365

  # Store performance in the current period.
  store_perf <- data |>
    dplyr::filter(.data$date >= p_start) |>
    dplyr::group_by(.data$store_nbr, .data$city, .data$type) |>
    dplyr::summarise(total_sales = sum(.data$sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total_sales))

  # Category growth: current vs previous period.
  family_growth <- data |>
    dplyr::group_by(.data$family) |>
    dplyr::summarise(
      biezacy = sum(.data$sales[.data$date >= p_start], na.rm = TRUE),
      poprzedni = sum(
        .data$sales[.data$date >= prev_start & .data$date <= prev_end],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      zmiana_pct = dplyr::if_else(
        .data$poprzedni > 0,
        (.data$biezacy - .data$poprzedni) / .data$poprzedni * 100,
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$zmiana_pct))

  # Year-over-year totals and means.
  yoy <- tibble::tibble(
    okres = c("Bie\u017c\u0105cy", "Rok wcze\u015bniej"),
    suma = c(
      sum(data$sales[data$date >= p_start], na.rm = TRUE),
      sum(data$sales[data$date >= yoy_start & data$date <= yoy_end], na.rm = TRUE)
    ),
    srednia = c(
      mean(data$sales[data$date >= p_start], na.rm = TRUE),
      mean(data$sales[data$date >= yoy_start & data$date <= yoy_end], na.rm = TRUE)
    )
  )
  yoy_change_pct <- if (yoy$suma[2] > 0) {
    (yoy$suma[1] - yoy$suma[2]) / yoy$suma[2] * 100
  } else {
    NA_real_
  }

  # Promotion and holiday effectiveness, reusing compute_sales_metrics().
  metrics_family <- compute_sales_metrics(data, group_by = "family")
  promo_rank <- metrics_family |>
    dplyr::arrange(dplyr::desc(.data$promo_uplift_pct)) |>
    dplyr::select("family", "promo_uplift_pct") |>
    head(5)

  has_holiday <- "is_holiday" %in% names(data)
  holiday_rank <- if (has_holiday) {
    metrics_family |>
      dplyr::filter(!is.na(.data$holiday_uplift_pct)) |>
      dplyr::arrange(dplyr::desc(.data$holiday_uplift_pct)) |>
      dplyr::select("family", "holiday_uplift_pct") |>
      head(5)
  } else {
    NULL
  }

  # Revenue concentration.
  store_total_all <- data |>
    dplyr::group_by(.data$store_nbr) |>
    dplyr::summarise(total = sum(.data$sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total))
  grand_total <- sum(store_total_all$total)
  top5_share <- if (grand_total > 0) {
    sum(head(store_total_all$total, 5)) / grand_total * 100
  } else {
    NA_real_
  }
  hhi <- if (grand_total > 0) {
    sum((store_total_all$total / grand_total)^2)
  } else {
    NA_real_
  }

  # Sudden-drop alerts: weekly WoW drop > 30% within the last two weeks.
  alerts <- data |>
    dplyr::filter(.data$date > max_date - 21) |>
    dplyr::mutate(tydzien = lubridate::floor_date(.data$date, "week")) |>
    dplyr::group_by(.data$store_nbr, .data$family, .data$tydzien) |>
    dplyr::summarise(s = sum(.data$sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$store_nbr, .data$family, .data$tydzien) |>
    dplyr::group_by(.data$store_nbr, .data$family) |>
    dplyr::mutate(zmiana_wow = (.data$s - dplyr::lag(.data$s)) /
      dplyr::lag(.data$s) * 100) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$zmiana_wow), .data$zmiana_wow < -30) |>
    dplyr::arrange(.data$zmiana_wow)

  structure(
    list(
      period_days = period_days,
      date_range = c(min(data$date, na.rm = TRUE), max_date),
      best_store = utils::head(store_perf, 1),
      worst_store = utils::tail(store_perf, 1),
      store_perf = store_perf,
      fastest_growing = utils::head(family_growth, 1),
      biggest_drop = utils::tail(family_growth, 1),
      family_growth = family_growth,
      yoy = yoy,
      yoy_change_pct = yoy_change_pct,
      promo_rank = promo_rank,
      holiday_rank = holiday_rank,
      top5_share = top5_share,
      hhi = hhi,
      alerts = alerts
    ),
    class = "mgmt_summary"
  )
}

#' @export
print.mgmt_summary <- function(x, ...) {
  cli::cli_h1("Podsumowanie mened\u017cerskie sprzeda\u017cy")
  cli::cli_text(
    "Okres analizy: ostatnie {.val {x$period_days}} dni ",
    "(dane do {.val {as.character(x$date_range[2])}})."
  )

  cli::cli_h2("Sklepy")
  cli::cli_alert_info(
    "Najlepszy sklep: nr {.val {x$best_store$store_nbr}} ({x$best_store$city}), ",
    "sprzeda\u017c {.val {fmt_num(x$best_store$total_sales)}}."
  )
  cli::cli_alert_info(
    "Najs\u0142abszy sklep: nr {.val {x$worst_store$store_nbr}} ({x$worst_store$city}), ",
    "sprzeda\u017c {.val {fmt_num(x$worst_store$total_sales)}}."
  )

  cli::cli_h2("Kategorie")
  cli::cli_alert_success(
    "Najszybciej rosn\u0105ca kategoria: {.val {x$fastest_growing$family}} ",
    "({fmt_num(x$fastest_growing$zmiana_pct, 1)}% wzgl. poprzedniego okresu)."
  )
  cli::cli_alert_danger(
    "Najwi\u0119kszy spadek: {.val {x$biggest_drop$family}} ",
    "({fmt_num(x$biggest_drop$zmiana_pct, 1)}%)."
  )

  cli::cli_h2("Por\u00f3wnanie rok do roku (YoY)")
  yoy_txt <- if (is.na(x$yoy_change_pct)) {
    "brak danych z poprzedniego roku"
  } else {
    paste0(fmt_num(x$yoy_change_pct, 1), "%")
  }
  cli::cli_text("Zmiana sumy sprzeda\u017cy YoY: {.val {yoy_txt}}.")
  print(x$yoy)

  cli::cli_h2("Skuteczno\u015b\u0107 promocji (top 5 kategorii)")
  print(x$promo_rank)

  if (!is.null(x$holiday_rank)) {
    cli::cli_h2("Efekt \u015bwi\u0105t (top kategorie wg holiday_uplift_pct)")
    print(x$holiday_rank)
  }

  cli::cli_h2("Koncentracja przychodu")
  cli::cli_text(
    "Udzia\u0142 top-5 sklep\u00f3w: {.val {fmt_num(x$top5_share, 1)}}%; ",
    "indeks HHI: {.val {fmt_num(x$hhi, 3)}}."
  )

  cli::cli_h2("Alerty (spadek tydzie\u0144-do-tygodnia > 30%)")
  if (nrow(x$alerts) == 0) {
    cli::cli_alert_success("Brak nag\u0142ych spadk\u00f3w w ostatnich 2 tygodniach.")
  } else {
    cli::cli_alert_warning("Wykryto {.val {nrow(x$alerts)}} serii z nag\u0142ym spadkiem:")
    print(utils::head(x$alerts, 10))
  }

  invisible(x)
}
