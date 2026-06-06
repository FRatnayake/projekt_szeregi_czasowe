#' Load and parse a holidays / events file
#'
#' Reads a `holidays_events.csv` file (Favorita schema), parses the date,
#' converts the `transferred` text column to a logical, and returns a tibble with
#' the S3 class `"holiday_data"` (which has a Polish-language [summary()] method).
#'
#' @param holidays_path Path to `holidays_events.csv`.
#'
#' @return A tibble of class `c("holiday_data", "tbl_df", "tbl", "data.frame")`
#'   with columns `date` (`Date`), `type`, `locale`, `locale_name`,
#'   `description` and `transferred` (`logical`).
#'
#' @details The `transferred` flag is important and easy to misread: when
#'   `transferred == TRUE` the nominal holiday on that date was **moved**, so that
#'   calendar day is **not** a day off. The real day off appears on a different
#'   date, recorded as a `Transfer` (or `Bridge`) type row. This is why
#'   [load_sales_data()] excludes `transferred == TRUE` rows when building the
#'   `is_holiday` flag.
#'
#' @examples
#' path <- system.file("extdata", "holidays_events.csv", package = "salesTS")
#' hd <- load_holiday_data(path)
#' summary(hd)
#'
#' @export
load_holiday_data <- function(holidays_path) {
  if (!is.character(holidays_path) || length(holidays_path) != 1L) {
    cli::cli_abort("Argument {.arg holidays_path} musi by\u0107 pojedyncz\u0105 \u015bcie\u017ck\u0105 (znakow\u0105).")
  }
  if (!file.exists(holidays_path)) {
    cli::cli_abort("Nie znaleziono pliku \u015bwi\u0105t: {.path {holidays_path}}.")
  }

  raw <- readr::read_csv(
    holidays_path,
    col_types = readr::cols(
      date = readr::col_date(),
      type = readr::col_character(),
      locale = readr::col_character(),
      locale_name = readr::col_character(),
      description = readr::col_character(),
      transferred = readr::col_character()
    ),
    progress = FALSE
  )

  check_columns(
    raw,
    c("date", "type", "locale", "locale_name", "description", "transferred"),
    arg = "holidays_path"
  )

  out <- raw |>
    dplyr::mutate(
      transferred = tolower(.data$transferred) %in% c("true", "t", "1")
    )

  structure(out, class = c("holiday_data", class(tibble::tibble())))
}

#' @export
#' @rdname load_holiday_data
#' @param object A `holiday_data` object.
#' @param ... Unused; for S3 compatibility.
summary.holiday_data <- function(object, ...) {
  by_type <- object |>
    dplyr::count(.data$type, name = "liczba") |>
    dplyr::arrange(dplyr::desc(.data$liczba)) |>
    tibble::as_tibble()
  by_locale <- object |>
    dplyr::count(.data$locale, name = "liczba") |>
    dplyr::arrange(dplyr::desc(.data$liczba)) |>
    tibble::as_tibble()
  n_transferred <- sum(object$transferred)

  cli::cli_h1("Podsumowanie danych o \u015bwi\u0119tach")
  cli::cli_text("Liczba wpis\u00f3w: {.val {nrow(object)}} w zakresie dat {.val {as.character(min(object$date))}} - {.val {as.character(max(object$date))}}.")
  cli::cli_text("\u015awi\u0105t przeniesionych (nie s\u0105 dniem wolnym): {.val {n_transferred}}.")
  cli::cli_h2("Rozk\u0142ad wg typu")
  print(by_type)
  cli::cli_h2("Rozk\u0142ad wg zasi\u0119gu (locale)")
  print(by_locale)

  invisible(list(by_type = by_type, by_locale = by_locale, n_transferred = n_transferred))
}

#' @export
print.holiday_data <- function(x, ...) {
  cli::cli_text("{.cls holiday_data}: {.val {nrow(x)}} wpis\u00f3w o \u015bwi\u0119tach/wydarzeniach.")
  NextMethod()
}
