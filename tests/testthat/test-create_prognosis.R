test_that("create_prognosis returns tidy ARIMA forecasts", {
  fc <- create_prognosis(
    sample_sales,
    series = list(store_nbr = 1, family = "BEVERAGES"),
    models = "arima",
    horizon = 14,
    backtest = TRUE
  )

  expect_s3_class(fc, "prognosis")
  expect_setequal(names(fc$forecast), c("date", "model", "forecast", "lo95", "hi95"))
  expect_equal(nrow(fc$forecast), 14)
  expect_s3_class(fc$forecast$date, "Date")
  expect_true(all(fc$forecast$hi95 >= fc$forecast$lo95))
  expect_s3_class(fc$backtest, "tbl_df")
  expect_setequal(names(fc$backtest), c("model", "RMSE", "MAE", "MAPE"))
  expect_identical(fc$best_model, "arima")
})

test_that("aggregating across the whole panel works when series is NULL", {
  fc <- create_prognosis(sample_sales, models = "arima", horizon = 7, backtest = FALSE)
  expect_equal(nrow(fc$forecast), 7)
  expect_null(fc$backtest)
})

test_that("plot.prognosis returns a ggplot", {
  fc <- create_prognosis(
    sample_sales,
    series = list(store_nbr = 1, family = "BEVERAGES"),
    models = "arima", horizon = 10, backtest = FALSE
  )
  expect_s3_class(plot(fc), "ggplot")
})

test_that("prophet is used when installed", {
  skip_if_not_installed("prophet")
  fc <- create_prognosis(
    sample_sales,
    series = list(store_nbr = 1, family = "BEVERAGES"),
    models = c("arima", "prophet"),
    horizon = 10, backtest = TRUE
  )
  expect_true("prophet" %in% fc$forecast$model)
  expect_true("prophet" %in% fc$backtest$model)
})

test_that("create_prognosis validates inputs and empty series", {
  expect_error(
    create_prognosis(sample_sales, test_size = 1.5),
    class = "rlang_error"
  )
  expect_error(
    create_prognosis(sample_sales, series = list(store_nbr = 999)),
    class = "rlang_error"
  )
})
