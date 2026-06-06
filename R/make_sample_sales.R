#' Generate a synthetic retail sales panel matching the Favorita schema
#'
#' Produces a reproducible, self-contained sales panel with the exact columns of
#' the raw `train.csv` (`id`, `date`, `store_nbr`, `family`, `sales`,
#' `onpromotion`) plus a separate store-metadata table. The series combine a
#' linear trend, weekly seasonality, yearly seasonality, a promotion effect and
#' noise. A handful of data-quality problems are deliberately injected (missing
#' values, duplicates, negative values, a multi-day gap) so that
#' [validate_sales_ts()] and [clean_sales_ts()] have something to detect and fix.
#'
#' @param n_stores Number of stores to generate (each gets random `type` and
#'   `city`/`state`).
#' @param n_families Number of product families (drawn from the real Favorita
#'   family names).
#' @param start Start date (a string or `Date`) of the panel.
#' @param days Number of consecutive days per series.
#' @param seed Random seed for reproducibility.
#'
#' @return A named list with two tibbles:
#'   \describe{
#'     \item{`sales`}{Panel with columns `id`, `date`, `store_nbr`, `family`,
#'       `sales`, `onpromotion` (schema-compatible with `train.csv`).}
#'     \item{`stores`}{Store metadata with columns `store_nbr`, `city`, `state`,
#'       `type`, `cluster`.}
#'   }
#'
#' @details The injected defects are intentional and documented so that the
#'   cleaning pipeline can be demonstrated end to end:
#'   * several `NA` values in `sales`,
#'   * 2-3 duplicated `(date, store_nbr, family)` rows,
#'   * a few negative `sales` values,
#'   * a multi-day gap removed from one series.
#'
#' @examples
#' sim <- make_sample_sales(n_stores = 3, n_families = 2, days = 90)
#' head(sim$sales)
#' sim$stores
#'
#' @export
make_sample_sales <- function(n_stores = 6,
                              n_families = 4,
                              start = "2015-01-01",
                              days = 365,
                              seed = 42) {
  check_count(n_stores, "n_stores")
  check_count(n_families, "n_families")
  check_count(days, "days")

  all_families <- c(
    "AUTOMOTIVE", "BABY CARE", "BEAUTY", "BEVERAGES", "BOOKS", "BREAD/BAKERY",
    "CELEBRATION", "CLEANING", "DAIRY", "DELI", "EGGS", "FROZEN FOODS",
    "GROCERY I", "GROCERY II", "HARDWARE", "HOME AND KITCHEN I",
    "HOME AND KITCHEN II", "HOME APPLIANCES", "HOME CARE", "LADIESWEAR",
    "LAWN AND GARDEN", "LINGERIE", "LIQUOR/WINE/BEER", "MAGAZINES", "MEATS",
    "PERSONAL CARE", "PET SUPPLIES", "PLAYERS AND ELECTRONICS", "POULTRY",
    "PREPARED FOODS", "PRODUCE", "SCHOOL AND OFFICE SUPPLIES", "SEAFOOD"
  )
  if (n_families > length(all_families)) {
    cli::cli_abort(
      "Argument {.arg n_families} nie mo\u017ce przekracza\u0107 {length(all_families)}."
    )
  }

  cities <- c(
    "Quito", "Guayaquil", "Cuenca", "Ambato", "Machala",
    "Manta", "Loja", "Santo Domingo"
  )
  states <- c(
    "Pichincha", "Guayas", "Azuay", "Tungurahua", "El Oro",
    "Manabi", "Loja", "Santo Domingo de los Tsachilas"
  )

  start <- as.Date(start)

  # Set the seed for reproducibility but restore the caller's RNG state on exit
  # so we do not clobber the user's random stream as a side effect.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      },
      add = TRUE
    )
  }
  set.seed(seed)

  store_ids <- seq_len(n_stores)
  city_idx <- sample(seq_along(cities), n_stores, replace = TRUE)
  stores <- tibble::tibble(
    store_nbr = store_ids,
    city = cities[city_idx],
    state = states[city_idx],
    type = sample(c("A", "B", "C", "D", "E"), n_stores, replace = TRUE),
    cluster = sample(1:17, n_stores, replace = TRUE)
  )

  families <- all_families[seq_len(n_families)]
  dates <- start + seq_len(days) - 1L

  # Per-(store, family) base level so series differ in magnitude.
  grid <- tidyr::expand_grid(store_nbr = store_ids, family = families)
  grid$base <- stats::runif(nrow(grid), 20, 120)
  grid$trend <- stats::runif(nrow(grid), -0.02, 0.08)
  grid$promo_rate <- stats::runif(nrow(grid), 0.05, 0.30)

  t <- seq_len(days)
  weekly <- function(d) 1 + 0.35 * sin(2 * pi * (lubridate::wday(d) - 1) / 7)
  yearly <- function(d) 1 + 0.20 * sin(2 * pi * lubridate::yday(d) / 365.25)

  build_series <- function(base, trend, promo_rate) {
    promo <- stats::rbinom(days, 1L, promo_rate)
    level <- base + trend * t
    seasonal <- weekly(dates) * yearly(dates)
    noise <- stats::rnorm(days, 0, 0.10 * base)
    sales <- level * seasonal * (1 + 0.25 * promo) + noise
    sales <- pmax(sales, 0)
    tibble::tibble(date = dates, sales = round(sales, 3), onpromotion = promo)
  }

  panel <- purrr::pmap(
    list(grid$store_nbr, grid$family, grid$base, grid$trend, grid$promo_rate),
    function(store_nbr, family, base, trend, promo_rate) {
      s <- build_series(base, trend, promo_rate)
      s$store_nbr <- store_nbr
      s$family <- family
      s
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$store_nbr, .data$family, .data$date) |>
    dplyr::mutate(id = dplyr::row_number(), .before = 1) |>
    dplyr::select("id", "date", "store_nbr", "family", "sales", "onpromotion")

  panel <- inject_defects(panel)

  list(sales = panel, stores = stores)
}

# Inject the deliberate data-quality problems described in the docs.
inject_defects <- function(panel) {
  n <- nrow(panel)

  # A few missing values in sales.
  na_idx <- sample(n, size = max(5L, round(0.002 * n)))
  panel$sales[na_idx] <- NA_real_

  # A few negative values (data-entry style errors).
  neg_idx <- sample(setdiff(seq_len(n), na_idx), size = 4L)
  panel$sales[neg_idx] <- -abs(panel$sales[neg_idx]) - 1

  # 2-3 duplicated (date, store_nbr, family) rows with a fresh id.
  dup_rows <- panel[sample(setdiff(seq_len(n), c(na_idx, neg_idx)), 3L), ]
  dup_rows$id <- max(panel$id) + seq_len(nrow(dup_rows))
  dup_rows$sales <- dup_rows$sales * 0.5
  panel <- dplyr::bind_rows(panel, dup_rows)

  # A multi-day gap in the first series: drop ~5 consecutive days.
  first_key <- panel |>
    dplyr::slice(1) |>
    dplyr::select("store_nbr", "family")
  series_dates <- panel |>
    dplyr::semi_join(first_key, by = c("store_nbr", "family")) |>
    dplyr::arrange(.data$date) |>
    dplyr::pull(.data$date)
  if (length(series_dates) > 40L) {
    gap_dates <- series_dates[20:24]
    drop <- with(panel, panel$store_nbr == first_key$store_nbr &
      panel$family == first_key$family & panel$date %in% gap_dates)
    panel <- panel[!drop, ]
  }

  panel |>
    dplyr::arrange(.data$store_nbr, .data$family, .data$date)
}
