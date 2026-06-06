#' Clean a sales time-series table
#'
#' Resolves the data-quality problems detected by [validate_sales_ts()]: it fills
#' or removes missing `sales`, collapses duplicated keys, sorts the data and can
#' optionally aggregate to a coarser frequency. The number of rows affected by
#' each step is recorded in the `"cleaning_log"` attribute of the result.
#'
#' @param data A sales tibble with `date`, `store_nbr`, `family`, `sales`,
#'   `onpromotion`.
#' @param missing How to handle missing `sales`: `"interpolate"` (linear, via
#'   [zoo::na.approx()]), `"locf"` (last observation carried forward), `"zero"`
#'   or `"drop"` (remove the rows).
#' @param dedupe How to collapse duplicated `(date, store_nbr, family)` rows:
#'   `"sum"`, `"mean"` or `"first"`. `onpromotion` is always taken as the maximum
#'   within the duplicate group.
#' @param sort Logical; sort by `(store_nbr, family, date)` (default `TRUE`).
#' @param aggregate Optional `"week"` or `"month"` to aggregate the series
#'   (sales summed, `onpromotion` averaged into a share, `is_holiday` reduced
#'   with `max` when present).
#'
#' @return A cleaned tibble carrying a `"cleaning_log"` attribute (a tibble of
#'   per-step row counts). Retrieve it with `attr(result, "cleaning_log")`.
#'
#' @details Steps run in this order: de-duplicate, handle missing values, sort,
#'   and finally aggregate. Interpolation and LOCF are applied **within** each
#'   `store_nbr x family` series after sorting, so values never leak across
#'   series.
#'
#' @examples
#' cleaned <- clean_sales_ts(sample_sales, missing = "interpolate", dedupe = "sum")
#' attr(cleaned, "cleaning_log")
#'
#' # Aggregate to weekly frequency.
#' weekly <- clean_sales_ts(sample_sales, aggregate = "week")
#'
#' @export
clean_sales_ts <- function(data,
                           missing = c("interpolate", "locf", "zero", "drop"),
                           dedupe = c("sum", "mean", "first"),
                           sort = TRUE,
                           aggregate = NULL) {
  check_columns(data, c("date", "store_nbr", "family", "sales", "onpromotion"))
  missing <- rlang::arg_match(missing)
  dedupe <- rlang::arg_match(dedupe)
  if (!is.null(aggregate)) {
    aggregate <- rlang::arg_match(aggregate, c("week", "month"))
  }

  log <- list()
  n_start <- nrow(data)

  # --- Step 1: de-duplicate keys -------------------------------------------
  key <- c("date", "store_nbr", "family")
  data <- dedupe_keys(data, key, dedupe)
  log$duplikaty_usuniete <- n_start - nrow(data)

  # --- Step 2: handle missing sales ----------------------------------------
  n_missing_before <- sum(is.na(data$sales))
  if (missing == "drop") {
    data <- dplyr::filter(data, !is.na(.data$sales))
    log$wiersze_usuniete_NA <- n_missing_before
    log$wartosci_NA_uzupelnione <- 0L
  } else {
    data <- data |>
      dplyr::arrange(.data$store_nbr, .data$family, .data$date) |>
      dplyr::group_by(.data$store_nbr, .data$family) |>
      dplyr::mutate(sales = fill_missing(.data$sales, missing)) |>
      dplyr::ungroup()
    log$wiersze_usuniete_NA <- 0L
    log$wartosci_NA_uzupelnione <- n_missing_before - sum(is.na(data$sales))
  }

  # --- Step 3: sort ---------------------------------------------------------
  if (sort) {
    data <- dplyr::arrange(data, .data$store_nbr, .data$family, .data$date)
  }

  # --- Step 4: aggregate ----------------------------------------------------
  if (!is.null(aggregate)) {
    n_before_agg <- nrow(data)
    data <- aggregate_series(data, aggregate)
    log$wiersze_po_agregacji <- nrow(data)
    log$wiersze_przed_agregacja <- n_before_agg
  }

  cleaning_log <- tibble::tibble(
    krok = names(log),
    liczba = unlist(log, use.names = FALSE)
  )
  attr(data, "cleaning_log") <- cleaning_log
  data
}

# Collapse duplicated keys, preserving metadata columns via `first`.
# Only rows belonging to a duplicated key are aggregated; unique rows are left
# untouched so that genuine NA values survive for the missing-value step.
dedupe_keys <- function(data, key, dedupe) {
  dup_mask <- duplicated(data[key]) | duplicated(data[key], fromLast = TRUE)
  if (!any(dup_mask)) {
    return(data)
  }
  original_order <- names(data)
  other_cols <- setdiff(names(data), c(key, "sales", "onpromotion"))
  combine <- switch(dedupe,
    sum = function(x) sum(x, na.rm = TRUE),
    mean = function(x) mean(x, na.rm = TRUE),
    first = function(x) dplyr::first(x)
  )

  collapsed <- data[dup_mask, ] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(
      sales = combine(.data$sales),
      onpromotion = max_promo(.data$onpromotion),
      dplyr::across(dplyr::all_of(other_cols), dplyr::first),
      .groups = "drop"
    )
  out <- dplyr::bind_rows(data[!dup_mask, ], collapsed)
  out[intersect(original_order, names(out))]
}

# Robust max for the integer 0/1 promotion flag (NA-safe).
max_promo <- function(p) {
  p <- p[!is.na(p)]
  if (length(p) == 0L) NA_integer_ else as.integer(max(p))
}

# Fill NA within a single series according to `method`.
fill_missing <- function(x, method) {
  if (all(is.na(x))) {
    return(x)
  }
  switch(method,
    zero = dplyr::coalesce(x, 0),
    locf = {
      x <- zoo::na.locf(x, na.rm = FALSE)
      zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)
    },
    interpolate = {
      if (sum(!is.na(x)) < 2L) {
        zoo::na.locf(zoo::na.locf(x, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE)
      } else {
        as.numeric(zoo::na.approx(x, na.rm = FALSE, rule = 2))
      }
    }
  )
}

# Aggregate to weekly / monthly frequency.
aggregate_series <- function(data, aggregate) {
  has_holiday <- "is_holiday" %in% names(data)
  reserved <- c("date", "store_nbr", "family", "sales", "onpromotion", "is_holiday")
  meta_cols <- setdiff(names(data), reserved)

  out <- data |>
    dplyr::mutate(date = lubridate::floor_date(.data$date, unit = aggregate)) |>
    dplyr::group_by(.data$store_nbr, .data$family, .data$date)

  out <- out |>
    dplyr::summarise(
      sales = sum(.data$sales, na.rm = TRUE),
      onpromotion = mean(.data$onpromotion, na.rm = TRUE),
      is_holiday = if (has_holiday) as.logical(max(.data$is_holiday, na.rm = TRUE)) else NULL,
      dplyr::across(dplyr::all_of(meta_cols), dplyr::first),
      .groups = "drop"
    )
  out
}
