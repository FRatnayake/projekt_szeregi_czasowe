test_that("create_management_summary returns an mgmt_summary with the key parts", {
  joined <- sample_joined()
  ms <- create_management_summary(joined, period_days = 30)

  expect_s3_class(ms, "mgmt_summary")
  expect_s3_class(ms$best_store, "tbl_df")
  expect_s3_class(ms$worst_store, "tbl_df")
  expect_s3_class(ms$promo_rank, "tbl_df")
  expect_true(is.numeric(ms$top5_share))
  expect_true(is.numeric(ms$hhi))
  expect_s3_class(ms$alerts, "tbl_df")
})

test_that("printing the summary works without error", {
  joined <- sample_joined()
  ms <- create_management_summary(joined, period_days = 30)
  # cli routes most output to the message stream, so capture both.
  out <- capture.output(print(ms))
  msg <- capture.output(print(ms), type = "message")
  expect_match(paste(c(out, msg), collapse = "\n"), "Podsumowanie")
})

test_that("create_management_summary requires store metadata", {
  expect_error(
    create_management_summary(sample_sales),
    class = "rlang_error"
  )
})
