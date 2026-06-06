test_that("make_sample_sales returns the documented schema", {
  sim <- make_sample_sales(n_stores = 3, n_families = 2, days = 60, seed = 1)

  expect_named(sim, c("sales", "stores"))
  expect_setequal(
    names(sim$sales),
    c("id", "date", "store_nbr", "family", "sales", "onpromotion")
  )
  expect_setequal(
    names(sim$stores),
    c("store_nbr", "city", "state", "type", "cluster")
  )
  expect_s3_class(sim$sales$date, "Date")
  expect_type(sim$sales$sales, "double")
  expect_equal(nrow(sim$stores), 3)
  expect_true(all(sim$stores$type %in% c("A", "B", "C", "D", "E")))
})

test_that("make_sample_sales injects the documented defects", {
  sim <- make_sample_sales(n_stores = 3, n_families = 2, days = 120, seed = 7)
  s <- sim$sales

  expect_true(any(is.na(s$sales)))
  expect_true(any(s$sales < 0, na.rm = TRUE))
  dup_keys <- s |>
    dplyr::count(.data$date, .data$store_nbr, .data$family) |>
    dplyr::filter(.data$n > 1)
  expect_gt(nrow(dup_keys), 0)
})

test_that("make_sample_sales is reproducible and validates inputs", {
  a <- make_sample_sales(seed = 42, days = 30)
  b <- make_sample_sales(seed = 42, days = 30)
  expect_equal(a$sales, b$sales)

  expect_error(make_sample_sales(n_families = 999), class = "rlang_error")
})
