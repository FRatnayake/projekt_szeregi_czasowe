# Internal helpers shared across the package. None of these are exported.
# Documentation is intentionally lightweight (internal use only); the public
# surface is documented on the exported functions.

# Schema of the raw Favorita training file, used for validation everywhere.
.sales_required_cols <- c("id", "date", "store_nbr", "family", "sales", "onpromotion")

#' Validate that a data frame contains the required columns
#'
#' @param data A data frame / tibble.
#' @param required Character vector of required column names.
#' @param arg Name of the argument being validated (for the message).
#' @param call The calling environment, forwarded to [cli::cli_abort()].
#' @return Invisibly `TRUE`; aborts otherwise.
#' @keywords internal
#' @noRd
check_columns <- function(data, required, arg = "data", call = rlang::caller_env()) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "Argument {.arg {arg}} musi by\u0107 ramk\u0105 danych (data.frame / tibble).",
      call = call
    )
  }
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "Brakuje wymaganych kolumn w {.arg {arg}}.",
        "x" = "Nie znaleziono: {.val {missing}}.",
        "i" = "Dost\u0119pne kolumny: {.val {names(data)}}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

#' Assert that a value is a single positive scalar
#' @keywords internal
#' @noRd
check_count <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1) {
    cli::cli_abort(
      "Argument {.arg {arg}} musi by\u0107 pojedyncz\u0105 liczb\u0105 ca\u0142kowit\u0105 >= 1.",
      call = call
    )
  }
  invisible(TRUE)
}

#' Detect the dominant sampling frequency of a vector of dates
#'
#' Uses the median day-to-day gap of the (sorted, unique) dates and maps it to
#' a calendar frequency label.
#'
#' @param dates A `Date` vector.
#' @return One of `"day"`, `"week"`, `"month"`.
#' @keywords internal
#' @noRd
detect_frequency <- function(dates) {
  dates <- sort(unique(dates[!is.na(dates)]))
  if (length(dates) < 2L) {
    return("day")
  }
  gap <- stats::median(as.numeric(diff(dates)))
  if (gap <= 3) {
    "day"
  } else if (gap <= 16) {
    "week"
  } else {
    "month"
  }
}

#' Detect indices of local maxima ("peaks") in a numeric series
#'
#' A point is a peak when it is strictly greater than every other point inside a
#' symmetric window of half-width `min_gap`. Because the comparison window is
#' wider than `min_gap`, detected peaks are naturally separated by at least
#' `min_gap` observations.
#'
#' @param x Numeric vector (e.g. a daily sales series).
#' @param min_gap Half-width of the comparison window / minimum spacing.
#' @return Integer vector of peak indices (possibly empty).
#' @keywords internal
#' @noRd
detect_peaks <- function(x, min_gap = 3) {
  n <- length(x)
  if (n < 3L) {
    return(integer(0))
  }
  peaks <- integer(0)
  for (i in seq_len(n)) {
    if (is.na(x[i])) next
    lo <- max(1L, i - min_gap)
    hi <- min(n, i + min_gap)
    idx <- setdiff(lo:hi, i)
    others <- x[idx]
    if (length(others) == 0L) next
    if (all(is.na(others))) next
    if (x[i] > max(others, na.rm = TRUE)) {
      peaks <- c(peaks, i)
    }
  }
  peaks
}

#' Mean spacing (in observations) between consecutive peaks
#' @keywords internal
#' @noRd
peak_distance <- function(x, min_gap = 3) {
  pk <- detect_peaks(x, min_gap = min_gap)
  if (length(pk) < 2L) {
    return(NA_real_)
  }
  mean(diff(pk))
}

#' Last value of a right-aligned rolling mean
#' @keywords internal
#' @noRd
last_rollmean <- function(x, window) {
  x <- x[!is.na(x)]
  if (length(x) < window) {
    return(if (length(x) == 0L) NA_real_ else mean(x))
  }
  ra <- zoo::rollmean(x, k = window, align = "right")
  ra[length(ra)]
}

#' Percentage uplift of one group's mean over another's
#'
#' @param value Numeric values.
#' @param flag Logical / 0-1 vector splitting `value` into the "treated" group
#'   (`TRUE`/`> 0`) and the baseline group.
#' @return Percentage difference of treated mean vs baseline mean, or `NA`.
#' @keywords internal
#' @noRd
group_uplift_pct <- function(value, flag) {
  flag <- as.logical(flag)
  treated <- value[flag & !is.na(value)]
  base <- value[!flag & !is.na(value)]
  if (length(treated) == 0L || length(base) == 0L) {
    return(NA_real_)
  }
  base_mean <- mean(base)
  if (is.na(base_mean) || base_mean == 0) {
    return(NA_real_)
  }
  (mean(treated) - base_mean) / base_mean * 100
}

#' Period-over-period growth (last vs previous month) as a percentage
#' @keywords internal
#' @noRd
growth_pop <- function(sales, date) {
  agg <- tibble::tibble(
    m = lubridate::floor_date(date, "month"),
    s = sales
  ) |>
    dplyr::group_by(.data$m) |>
    dplyr::summarise(s = sum(.data$s, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$m)
  if (nrow(agg) < 2L) {
    return(NA_real_)
  }
  n <- nrow(agg)
  prev <- agg$s[n - 1L]
  last <- agg$s[n]
  if (is.na(prev) || prev == 0) {
    return(NA_real_)
  }
  (last - prev) / prev * 100
}

#' Compound annual growth rate (percentage) of a monthly-aggregated series
#' @keywords internal
#' @noRd
cagr_pct <- function(sales, date) {
  years <- as.numeric(max(date, na.rm = TRUE) - min(date, na.rm = TRUE)) / 365.25
  if (!is.finite(years) || years <= 0) {
    return(NA_real_)
  }
  agg <- tibble::tibble(
    m = lubridate::floor_date(date, "month"),
    s = sales
  ) |>
    dplyr::group_by(.data$m) |>
    dplyr::summarise(s = sum(.data$s, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$m)
  if (nrow(agg) < 2L) {
    return(NA_real_)
  }
  begin <- agg$s[1L]
  end <- agg$s[nrow(agg)]
  if (is.na(begin) || begin <= 0 || is.na(end) || end <= 0) {
    return(NA_real_)
  }
  ((end / begin)^(1 / years) - 1) * 100
}

#' Slope of an ordinary least squares fit of value on time
#' @keywords internal
#' @noRd
trend_slope <- function(sales, date) {
  d <- as.numeric(date)
  ok <- !is.na(sales) & !is.na(d)
  if (sum(ok) < 2L || stats::var(d[ok]) == 0) {
    return(NA_real_)
  }
  unname(stats::coef(stats::lm(sales[ok] ~ d[ok]))[2L])
}

#' Attach store metadata to a sales table via a left join on store_nbr
#'
#' @param data Sales tibble.
#' @param stores Store metadata tibble (must contain `store_nbr`).
#' @param call Calling environment.
#' @keywords internal
#' @noRd
join_store_metadata <- function(data, stores, call = rlang::caller_env()) {
  check_columns(stores, "store_nbr", arg = "stores", call = call)
  dplyr::left_join(data, stores, by = "store_nbr")
}

#' Build per-row holiday flags from a holidays_events table
#'
#' Implements the locale logic: National holidays apply to every store,
#' Regional holidays to stores in the matching `state`, Local holidays to stores
#' in the matching `city`. Transferred holidays (`transferred == TRUE`) and
#' "Work Day" rows are not days off. Requires `city`/`state` columns on `data`
#' for regional/local matching; without them only national holidays are applied.
#'
#' @param data Sales tibble (ideally already joined with store metadata).
#' @param holidays A `holiday_data` tibble from [load_holiday_data()].
#' @return `data` with logical `is_holiday` and `holiday_description` columns.
#' @keywords internal
#' @noRd
attach_holiday_flags <- function(data, holidays) {
  off <- holidays |>
    dplyr::filter(
      .data$type %in% c("Holiday", "Transfer", "Additional", "Bridge"),
      !.data$transferred
    )

  collapse_desc <- function(x) paste(unique(x), collapse = "; ")

  nat <- off |>
    dplyr::filter(.data$locale == "National") |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(hd_nat = collapse_desc(.data$description), .groups = "drop")

  data <- dplyr::left_join(data, nat, by = "date")

  if ("state" %in% names(data)) {
    reg <- off |>
      dplyr::filter(.data$locale == "Regional") |>
      dplyr::transmute(.data$date, state = .data$locale_name, .data$description) |>
      dplyr::group_by(.data$date, .data$state) |>
      dplyr::summarise(hd_reg = collapse_desc(.data$description), .groups = "drop")
    data <- dplyr::left_join(data, reg, by = c("date", "state"))
  } else {
    data$hd_reg <- NA_character_
  }

  if ("city" %in% names(data)) {
    loc <- off |>
      dplyr::filter(.data$locale == "Local") |>
      dplyr::transmute(.data$date, city = .data$locale_name, .data$description) |>
      dplyr::group_by(.data$date, .data$city) |>
      dplyr::summarise(hd_loc = collapse_desc(.data$description), .groups = "drop")
    data <- dplyr::left_join(data, loc, by = c("date", "city"))
  } else {
    data$hd_loc <- NA_character_
  }

  data |>
    dplyr::mutate(
      is_holiday = !is.na(.data$hd_nat) | !is.na(.data$hd_reg) | !is.na(.data$hd_loc),
      holiday_description = dplyr::coalesce(.data$hd_nat, .data$hd_reg, .data$hd_loc)
    ) |>
    dplyr::select(-"hd_nat", -"hd_reg", -"hd_loc")
}

#' Format a number for Polish-language reports (space thousands separator)
#' @keywords internal
#' @noRd
fmt_num <- function(x, digits = 0) {
  formatC(round(x, digits), format = "f", digits = digits, big.mark = " ")
}
