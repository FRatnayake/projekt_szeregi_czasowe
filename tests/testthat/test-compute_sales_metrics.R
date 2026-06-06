test_that("compute_sales_metrics returns one tidy row per group with all metrics", {
  cleaned <- clean_sales_ts(sample_sales)
  m <- compute_sales_metrics(cleaned, group_by = c("store_nbr", "family"))

  expected_cols <- c(
    "store_nbr", "family", "total_sales", "mean_sales", "moving_avg_last",
    "volatility_sd", "volatility_cv", "promo_share", "peak_distance_days",
    "promo_uplift_pct", "growth_pop", "cagr", "trend_slope",
    "zero_sales_rate", "holiday_uplift_pct"
  )
  expect_setequal(names(m), expected_cols)
  n_groups <- dplyr::n_distinct(cleaned$store_nbr, cleaned$family)
  expect_equal(nrow(m), n_groups)
  expect_type(m$total_sales, "double")
})

test_that("holiday_uplift_pct is NA without is_holiday and numeric with it", {
  m_no <- compute_sales_metrics(clean_sales_ts(sample_sales), group_by = "store_nbr")
  expect_true(all(is.na(m_no$holiday_uplift_pct)))

  d <- loaded_with_holidays()
  m_yes <- compute_sales_metrics(d, group_by = "family")
  expect_true(any(!is.na(m_yes$holiday_uplift_pct)))
})

test_that("group_by accepts metadata columns and rejects unknown ones", {
  joined <- sample_joined()
  m <- compute_sales_metrics(joined, group_by = "type")
  expect_true("type" %in% names(m))

  expect_error(
    compute_sales_metrics(joined, group_by = "not_a_column"),
    class = "rlang_error"
  )
})
