#' Load retail sales data from the Favorita CSV files
#'
#' Reads `train.csv` with an explicit column specification, optionally filters by
#' store and/or family **before** any joins (to save memory on the ~3M-row file),
#' and optionally enriches the data with store metadata and holiday flags.
#'
#' @param train_path Path to `train.csv`.
#' @param stores_path Optional path to `stores.csv`. If supplied, store metadata
#'   (`city`, `state`, `type`, `cluster`) is left-joined on `store_nbr`.
#' @param holidays_path Optional path to `holidays_events.csv`. If supplied, an
#'   `is_holiday` flag and `holiday_description` are attached (see *Details*).
#' @param n_max Maximum number of rows to read from `train.csv` (passed to
#'   [readr::read_csv()]). Handy for fast iteration during development.
#' @param stores Optional integer vector of `store_nbr` values to keep.
#' @param families Optional character vector of `family` values to keep.
#'
#' @return A tibble of class `c("sales_data", ...)` with the raw columns plus,
#'   when the corresponding files are supplied, store-metadata columns and the
#'   logical `is_holiday` / `holiday_description` columns.
#'
#' @details
#' Performance: the column types are declared explicitly via
#' [readr::cols()] so `readr` does not waste time guessing, and the `stores` /
#' `families` filters are applied immediately after reading (before the joins) to
#' shrink the working set as early as possible.
#'
#' Holiday logic: a date is treated as a day off for a given store according to
#' its scope -- **National** holidays apply to every store, **Regional** holidays
#' to stores whose `state` matches `locale_name`, and **Local** holidays to
#' stores whose `city` matches `locale_name`. Rows with `transferred == TRUE` are
#' excluded because the holiday was moved to another date. Regional and local
#' matching require store metadata, so pass `stores_path` together with
#' `holidays_path` for full coverage; otherwise only national holidays are
#' flagged.
#'
#' @examples
#' train <- system.file("extdata", "train_sample.csv", package = "salesTS")
#' stores <- system.file("extdata", "stores.csv", package = "salesTS")
#' holidays <- system.file("extdata", "holidays_events.csv", package = "salesTS")
#'
#' # Filter to a couple of stores/families while reading.
#' df <- load_sales_data(
#'   train, stores, holidays,
#'   stores = 1:3,
#'   families = c("BEVERAGES", "PRODUCE")
#' )
#' df
#'
#' @export
load_sales_data <- function(train_path,
                            stores_path = NULL,
                            holidays_path = NULL,
                            n_max = Inf,
                            stores = NULL,
                            families = NULL) {
  if (!is.character(train_path) || length(train_path) != 1L) {
    cli::cli_abort("Argument {.arg train_path} musi by\u0107 pojedyncz\u0105 \u015bcie\u017ck\u0105 (znakow\u0105).")
  }
  if (!file.exists(train_path)) {
    cli::cli_abort("Nie znaleziono pliku treningowego: {.path {train_path}}.")
  }
  if (!is.null(stores) && !is.numeric(stores)) {
    cli::cli_abort("Argument {.arg stores} musi by\u0107 wektorem liczbowym (store_nbr) albo NULL.")
  }
  if (!is.null(families) && !is.character(families)) {
    cli::cli_abort("Argument {.arg families} musi by\u0107 wektorem znakowym (family) albo NULL.")
  }

  data <- readr::read_csv(
    train_path,
    col_types = readr::cols(
      id = readr::col_integer(),
      date = readr::col_date(),
      store_nbr = readr::col_integer(),
      family = readr::col_character(),
      sales = readr::col_double(),
      onpromotion = readr::col_integer()
    ),
    n_max = n_max,
    progress = FALSE
  )

  check_columns(data, .sales_required_cols, arg = "train_path")

  # Apply filters early (before joins) to minimise memory use on large files.
  if (!is.null(stores)) {
    data <- dplyr::filter(data, .data$store_nbr %in% stores)
  }
  if (!is.null(families)) {
    data <- dplyr::filter(data, .data$family %in% families)
  }

  if (!is.null(stores_path)) {
    if (!file.exists(stores_path)) {
      cli::cli_abort("Nie znaleziono pliku sklep\u00f3w: {.path {stores_path}}.")
    }
    store_meta <- readr::read_csv(
      stores_path,
      col_types = readr::cols(
        store_nbr = readr::col_integer(),
        city = readr::col_character(),
        state = readr::col_character(),
        type = readr::col_character(),
        cluster = readr::col_integer()
      ),
      progress = FALSE
    )
    check_columns(store_meta, c("store_nbr", "city", "state", "type", "cluster"),
      arg = "stores_path"
    )
    data <- join_store_metadata(data, store_meta)
  }

  if (!is.null(holidays_path)) {
    holidays <- load_holiday_data(holidays_path)
    data <- attach_holiday_flags(data, holidays)
  }

  new_sales_data(data)
}

# Constructor for the lightweight S3 wrapper.
new_sales_data <- function(data) {
  structure(data, class = c("sales_data", class(tibble::tibble())))
}

#' @export
print.sales_data <- function(x, ...) {
  extras <- intersect(c("city", "state", "type", "cluster", "is_holiday"), names(x))
  cli::cli_text(
    "{.cls sales_data}: {.val {nrow(x)}} wierszy, ",
    "{.val {dplyr::n_distinct(x$store_nbr)}} sklep(y), ",
    "{.val {dplyr::n_distinct(x$family)}} kategori(i)."
  )
  if (length(extras) > 0) {
    cli::cli_text("Kolumny dodatkowe: {.val {extras}}.")
  }
  NextMethod()
}
