test_that("validate_sales_ts returns a validation object and does not mutate data", {
  before <- sample_sales
  val <- validate_sales_ts(sample_sales)

  expect_s3_class(val, "sales_validation")
  expect_identical(sample_sales, before)
  expect_s3_class(summary(val), "tbl_df")
  expect_setequal(names(summary(val)), c("sprawdzenie", "liczba"))
})

test_that("validate_sales_ts detects the injected defects", {
  val <- validate_sales_ts(sample_sales)
  s <- summary(val)
  get_count <- function(label) s$liczba[s$sprawdzenie == label]

  expect_gt(get_count("Braki NA (suma)"), 0)
  expect_gt(get_count("Duplikaty klucza"), 0)
  expect_gt(get_count("Wartości ujemne"), 0)
  expect_gt(get_count("Serie z lukami"), 0)
})

test_that("a clean dataset reports no issues", {
  clean <- sample_sales |>
    dplyr::filter(!is.na(.data$sales), .data$sales >= 0) |>
    dplyr::distinct(.data$date, .data$store_nbr, .data$family, .keep_all = TRUE)
  val <- validate_sales_ts(clean)
  s <- summary(val)
  expect_equal(s$liczba[s$sprawdzenie == "Duplikaty klucza"], 0)
  expect_equal(s$liczba[s$sprawdzenie == "Braki NA (suma)"], 0)
})

test_that("validate_sales_ts validates inputs", {
  expect_error(validate_sales_ts(data.frame(x = 1)), class = "rlang_error")
})
