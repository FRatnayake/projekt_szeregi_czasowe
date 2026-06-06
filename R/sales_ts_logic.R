#' Apply one or more analysis functions over filtered sales data
#'
#' A higher-order helper that filters a sales table (by metadata columns and/or a
#' date range) and then applies a function -- or a named list of functions --
#' to the result, optionally split by group. It demonstrates functional
#' programming patterns: closures capturing the function set, partial application
#' of extra arguments via [purrr::partial()], and progress reporting via
#' [cli::cli_progress_along()].
#'
#' @param data A sales tibble, ideally enriched with store metadata so that
#'   metadata filters work.
#' @param .funs A single function or a (preferably named) list of functions, each
#'   taking a data frame as its first argument.
#' @param filters A named list of column/value filters, e.g.
#'   `list(city = "Quito", type = "A")`. Values may be vectors
#'   (`list(city = c("Quito", "Guayaquil"))`), matched with `%in%`.
#' @param time_range Optional length-2 vector `c(start, end)` of dates (or date
#'   strings) restricting `date`.
#' @param group_by Optional character vector of columns; when supplied the data
#'   is split into groups and every function is applied per group.
#' @param ... Extra arguments partially applied to every function in `.funs`.
#'
#' @return A named list. Without `group_by`, one element per function. With
#'   `group_by`, one element per group, each itself a named list of per-function
#'   results.
#'
#' @details If the filters remove every row, the function aborts with a
#'   diagnostic message reporting how many rows each individual filter discarded,
#'   making it easy to see which filter was too strict. A warning is emitted when
#'   `group_by` produces more than 20 groups (the per-group `map()` can then be
#'   slow).
#'
#' @examples
#' joined <- dplyr::left_join(sample_sales, sample_stores, by = "store_nbr")
#'
#' # Apply two metrics to one store type, with extra args partially applied.
#' res <- sales_ts_logic(
#'   joined,
#'   .funs = list(metryki = compute_sales_metrics),
#'   filters = list(type = "A"),
#'   group_by = "store_nbr"
#' )
#' names(res)
#'
#' @export
sales_ts_logic <- function(data,
                           .funs,
                           filters = list(),
                           time_range = NULL,
                           group_by = NULL,
                           ...) {
  if (!is.data.frame(data)) {
    cli::cli_abort("Argument {.arg data} musi by\u0107 ramk\u0105 danych.")
  }
  fun_list <- normalize_funs(.funs)
  if (!is.list(filters)) {
    cli::cli_abort("Argument {.arg filters} musi by\u0107 list\u0105 par kolumna = warto\u015b\u0107.")
  }
  if (!is.null(group_by)) {
    if (!is.character(group_by)) {
      cli::cli_abort("Argument {.arg group_by} musi by\u0107 wektorem znakowym albo NULL.")
    }
    check_columns(data, group_by, arg = "group_by")
  }

  # --- Filter with per-filter accounting (closure over `removed`) ----------
  removed <- integer(0)
  cur <- data
  for (nm in names(filters)) {
    if (!nm %in% names(cur)) {
      cli::cli_abort(c(
        "Filtr odwo\u0142uje si\u0119 do nieistniej\u0105cej kolumny {.field {nm}}.",
        "i" = "Dost\u0119pne kolumny: {.val {names(cur)}}."
      ))
    }
    before <- nrow(cur)
    cur <- dplyr::filter(cur, .data[[nm]] %in% filters[[nm]])
    removed[nm] <- before - nrow(cur)
  }
  if (!is.null(time_range)) {
    if (length(time_range) != 2L) {
      cli::cli_abort("Argument {.arg time_range} musi mie\u0107 dok\u0142adnie 2 elementy: c(start, koniec).")
    }
    rng <- as.Date(time_range)
    before <- nrow(cur)
    cur <- dplyr::filter(cur, .data$date >= rng[1] & .data$date <= rng[2])
    removed["time_range"] <- before - nrow(cur)
  }

  if (nrow(cur) == 0L) {
    detail <- paste0(names(removed), ": -", removed, " wierszy")
    cli::cli_abort(c(
      "Po zastosowaniu filtr\u00f3w nie pozosta\u0142 \u017caden wiersz.",
      "x" = "Wk\u0142ad poszczeg\u00f3lnych filtr\u00f3w:",
      stats::setNames(detail, rep("*", length(detail)))
    ))
  }

  # --- Partial application of `...` to every function ----------------------
  dots <- list(...)
  funs <- purrr::map(fun_list, function(f) {
    if (length(dots) > 0) purrr::partial(f, !!!dots) else f
  })

  # --- Apply, optionally per group, with a progress bar -------------------
  if (is.null(group_by)) {
    runner <- function(df) purrr::map(funs, function(f) f(df))
    return(runner(cur))
  }

  groups <- cur |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by)))
  keys <- dplyr::group_keys(groups)
  group_names <- apply(keys, 1L, function(r) paste(r, collapse = " | "))
  splits <- dplyr::group_split(groups)
  names(splits) <- group_names

  if (length(splits) > 20L) {
    cli::cli_warn(
      "Przetwarzanie {.val {length(splits)}} grup - to mo\u017ce chwil\u0119 potrwa\u0107."
    )
  }

  runner <- function(df) purrr::map(funs, function(f) f(df))
  out <- purrr::map(
    cli::cli_progress_along(splits, name = "Przetwarzanie grup"),
    function(i) runner(splits[[i]])
  )
  names(out) <- names(splits)
  out
}

# Coerce `.funs` into a named list of functions, validating each element.
normalize_funs <- function(.funs, call = rlang::caller_env()) {
  if (is.function(.funs)) {
    return(list(wynik = .funs))
  }
  if (!is.list(.funs) || length(.funs) == 0L) {
    cli::cli_abort(
      "Argument {.arg .funs} musi by\u0107 funkcj\u0105 lub niepust\u0105 list\u0105 funkcji.",
      call = call
    )
  }
  if (!all(purrr::map_lgl(.funs, is.function))) {
    cli::cli_abort("Wszystkie elementy {.arg .funs} musz\u0105 by\u0107 funkcjami.", call = call)
  }
  nms <- rlang::names2(.funs)
  nms[nms == ""] <- paste0("funkcja_", seq_along(.funs))[nms == ""]
  rlang::set_names(.funs, nms)
}
