test_that("load_holiday_data parses the schema and transferred flag", {
  hd <- load_holiday_data(extdata_path("holidays_events.csv"))

  expect_s3_class(hd, "holiday_data")
  expect_setequal(
    names(hd),
    c("date", "type", "locale", "locale_name", "description", "transferred")
  )
  expect_s3_class(hd$date, "Date")
  expect_type(hd$transferred, "logical")
  expect_true(any(hd$transferred))
})

test_that("summary.holiday_data returns type/locale breakdowns", {
  hd <- load_holiday_data(extdata_path("holidays_events.csv"))
  res <- summary(hd)
  expect_named(res, c("by_type", "by_locale", "n_transferred"))
  expect_s3_class(res$by_type, "tbl_df")
  expect_equal(sum(res$by_type$liczba), nrow(hd))
})

test_that("load_holiday_data validates its argument", {
  expect_error(load_holiday_data(123), class = "rlang_error")
  expect_error(load_holiday_data("missing.csv"), class = "rlang_error")
})
