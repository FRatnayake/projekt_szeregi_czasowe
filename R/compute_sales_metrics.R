#' Compute per-group sales metrics
#'
#' Aggregates a sales table into one tidy row per group, computing a rich set of
#' descriptive, volatility, promotion, growth and (optionally) holiday metrics.
#'
#' @param data A sales tibble. Must contain `date`, `sales`, `onpromotion` plus
#'   any columns named in `group_by`.
#' @param group_by Character vector of grouping columns, any subset of
#'   `{store_nbr, family, city, state, type, cluster}`.
#' @param window Integer window for the trailing moving average (default `7`).
#'
#' @return A tidy tibble with one row per group and the following columns:
#'   \describe{
#'     \item{`total_sales`}{Total sales.}
#'     \item{`mean_sales`}{Mean daily sales.}
#'     \item{`moving_avg_last`}{Last value of the trailing `window`-day moving average.}
#'     \item{`volatility_sd`}{Standard deviation of sales.}
#'     \item{`volatility_cv`}{Coefficient of variation (`sd / mean`).}
#'     \item{`promo_share`}{Percentage of rows with `onpromotion > 0`.}
#'     \item{`peak_distance_days`}{Mean spacing between local sales peaks.}
#'     \item{`promo_uplift_pct`}{Percentage uplift of mean sales on promo days vs non-promo.}
#'     \item{`growth_pop`}{Period-over-period growth (last vs previous month), in percent.}
#'     \item{`cagr`}{Compound annual growth rate over the series, in percent.}
#'     \item{`trend_slope`}{Slope of `lm(sales ~ as.numeric(date))`.}
#'     \item{`zero_sales_rate`}{Share of days with `sales == 0` (stockout proxy).}
#'     \item{`holiday_uplift_pct`}{Percentage uplift on holidays vs non-holidays
#'       (`NA` when `is_holiday` is absent).}
#'   }
#'
#' @details Peaks are found with an internal helper that flags points strictly
#'   greater than their neighbours within a `min_gap` window; `peak_distance_days`
#'   is the mean gap between consecutive peaks (`NA` with fewer than two peaks).
#'
#' @examples
#' compute_sales_metrics(sample_sales, group_by = c("store_nbr", "family"))
#'
#' # Group by an arbitrary metadata column after a join.
#' joined <- dplyr::left_join(sample_sales, sample_stores, by = "store_nbr")
#' compute_sales_metrics(joined, group_by = "type")
#'
#' @export
compute_sales_metrics <- function(data,
                                  group_by = c("store_nbr", "family"),
                                  window = 7L) {
  check_columns(data, c("date", "sales", "onpromotion"))
  if (!is.character(group_by) || length(group_by) == 0L) {
    cli::cli_abort("Argument {.arg group_by} musi by\u0107 niepustym wektorem znakowym.")
  }
  allowed <- c("store_nbr", "family", "city", "state", "type", "cluster")
  bad <- setdiff(group_by, allowed)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Niedozwolone kolumny w {.arg group_by}: {.val {bad}}.",
      "i" = "Dozwolone: {.val {allowed}}."
    ))
  }
  check_columns(data, group_by, arg = "group_by")
  check_count(window, "window")

  has_holiday <- "is_holiday" %in% names(data)

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) |>
    dplyr::summarise(
      total_sales = sum(.data$sales, na.rm = TRUE),
      mean_sales = mean(.data$sales, na.rm = TRUE),
      moving_avg_last = last_rollmean(.data$sales, window),
      volatility_sd = stats::sd(.data$sales, na.rm = TRUE),
      volatility_cv = stats::sd(.data$sales, na.rm = TRUE) /
        mean(.data$sales, na.rm = TRUE),
      promo_share = mean(.data$onpromotion > 0, na.rm = TRUE) * 100,
      peak_distance_days = peak_distance(.data$sales),
      promo_uplift_pct = group_uplift_pct(.data$sales, .data$onpromotion > 0),
      growth_pop = growth_pop(.data$sales, .data$date),
      cagr = cagr_pct(.data$sales, .data$date),
      trend_slope = trend_slope(.data$sales, .data$date),
      zero_sales_rate = mean(.data$sales == 0, na.rm = TRUE),
      holiday_uplift_pct = if (has_holiday) {
        group_uplift_pct(.data$sales, .data$is_holiday)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
}
