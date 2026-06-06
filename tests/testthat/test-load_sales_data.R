test_that("load_sales_data reads the schema and returns a sales_data object", {
  d <- load_sales_data(extdata_path("train_sample.csv"))

  expect_s3_class(d, "sales_data")
  expect_true(all(c("id", "date", "store_nbr", "family", "sales", "onpromotion") %in% names(d)))
  expect_s3_class(d$date, "Date")
  expect_type(d$store_nbr, "integer")
})

test_that("store/family filters and n_max are applied while reading", {
  d <- load_sales_data(
    extdata_path("train_sample.csv"),
    stores = 1:2,
    families = "BEVERAGES",
    n_max = 5000
  )
  expect_setequal(unique(d$store_nbr), 1:2)
  expect_setequal(unique(d$family), "BEVERAGES")
})

test_that("joining stores and holidays adds the expected columns", {
  d <- loaded_with_holidays()
  expect_true(all(c("city", "state", "type", "cluster") %in% names(d)))
  expect_true("is_holiday" %in% names(d))
  expect_type(d$is_holiday, "logical")
  expect_true(any(d$is_holiday))
})

test_that("transferred holidays are not treated as days off", {
  d <- loaded_with_holidays()
  # 2015-05-24 Batalla de Pichincha is transferred == TRUE -> not a holiday.
  may24 <- dplyr::filter(d, .data$date == as.Date("2015-05-24"))
  expect_true(nrow(may24) > 0)
  expect_false(any(may24$is_holiday))
})

test_that("load_sales_data validates its arguments", {
  expect_error(load_sales_data(123), class = "rlang_error")
  expect_error(load_sales_data("does-not-exist.csv"), class = "rlang_error")
})
