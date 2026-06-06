test_that("clean_sales_ts removes the injected defects", {
  cleaned <- clean_sales_ts(sample_sales, missing = "interpolate", dedupe = "sum")

  expect_false(any(is.na(cleaned$sales)))
  dup_keys <- cleaned |>
    dplyr::count(.data$date, .data$store_nbr, .data$family) |>
    dplyr::filter(.data$n > 1)
  expect_equal(nrow(dup_keys), 0)
  expect_s3_class(attr(cleaned, "cleaning_log"), "tbl_df")
})

test_that("missing strategies behave as documented", {
  dropped <- clean_sales_ts(sample_sales, missing = "drop")
  zeroed <- clean_sales_ts(sample_sales, missing = "zero")

  expect_false(any(is.na(dropped$sales)))
  expect_false(any(is.na(zeroed$sales)))
  # dropping removes rows; zero-filling keeps them.
  expect_lt(nrow(dropped), nrow(zeroed))
})

test_that("aggregation collapses to a coarser frequency", {
  weekly <- clean_sales_ts(sample_sales, aggregate = "week")
  daily <- clean_sales_ts(sample_sales)

  expect_lt(nrow(weekly), nrow(daily))
  expect_true(all(c("store_nbr", "family", "date", "sales", "onpromotion") %in% names(weekly)))
  # weekly sales total should be close to the daily total (after cleaning).
  expect_equal(sum(weekly$sales), sum(daily$sales), tolerance = 0.01)
})

test_that("clean_sales_ts validates inputs", {
  expect_error(clean_sales_ts(sample_sales, missing = "nonsense"), class = "rlang_error")
  expect_error(clean_sales_ts(data.frame(x = 1)), class = "rlang_error")
})
