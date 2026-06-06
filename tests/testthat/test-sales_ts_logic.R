test_that("sales_ts_logic applies a named list of functions", {
  joined <- sample_joined()
  res <- sales_ts_logic(
    joined,
    .funs = list(
      metryki = function(d) compute_sales_metrics(d, group_by = "family"),
      liczba = nrow
    )
  )
  expect_named(res, c("metryki", "liczba"))
  expect_s3_class(res$metryki, "tbl_df")
  expect_type(res$liczba, "integer")
})

test_that("filters and group_by work together", {
  joined <- sample_joined()
  type1 <- joined$type[1]
  res <- sales_ts_logic(
    joined,
    .funs = list(n = nrow),
    filters = list(type = type1),
    group_by = "store_nbr"
  )
  expect_true(length(res) >= 1)
  # every group result is itself a named list of function outputs.
  expect_named(res[[1]], "n")
})

test_that("partial application forwards extra arguments", {
  joined <- sample_joined()
  res <- sales_ts_logic(
    joined,
    .funs = list(m = compute_sales_metrics),
    group_by = "type"
  )
  expect_true(all(purrr::map_lgl(res, ~ inherits(.x$m, "tbl_df"))))
})

test_that("empty filter result aborts with a diagnostic", {
  joined <- sample_joined()
  expect_error(
    sales_ts_logic(joined, .funs = nrow, filters = list(type = "ZZZ")),
    class = "rlang_error"
  )
})

test_that("invalid .funs is rejected", {
  expect_error(sales_ts_logic(sample_sales, .funs = 42), class = "rlang_error")
})
