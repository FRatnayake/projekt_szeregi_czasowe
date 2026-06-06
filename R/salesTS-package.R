#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data .env :=
#' @importFrom stats coef lm median predict sd
#' @importFrom utils head tail
## usethis namespace: end
NULL

# Operator re-export so internal code can use the native-pipe-friendly `%||%`
# without depending on base R >= 4.4. Imported from rlang.
#' @importFrom rlang %||%
NULL
