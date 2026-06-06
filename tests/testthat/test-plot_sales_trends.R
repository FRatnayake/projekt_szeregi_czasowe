test_that("each plot type returns a ggplot object", {
  cleaned <- clean_sales_ts(sample_sales)
  for (ty in c("line", "decomposition", "heatmap", "promo")) {
    p <- plot_sales_trends(cleaned, type = ty)
    expect_s3_class(p, "ggplot")
  }
})

test_that("holiday plot requires is_holiday", {
  expect_error(
    plot_sales_trends(clean_sales_ts(sample_sales), type = "holiday"),
    class = "rlang_error"
  )
  d <- loaded_with_holidays()
  expect_s3_class(plot_sales_trends(d, type = "holiday"), "ggplot")
})

test_that("decomposition errors on a too-short series", {
  short <- sample_sales |>
    dplyr::filter(.data$store_nbr == 1, .data$family == "BEVERAGES") |>
    dplyr::arrange(.data$date) |>
    head(8)
  expect_error(
    plot_sales_trends(short, type = "decomposition"),
    class = "rlang_error"
  )
})

test_that("interactive degrades gracefully when plotly is missing", {
  skip_if(requireNamespace("plotly", quietly = TRUE), "plotly is installed")
  expect_warning(
    p <- plot_sales_trends(clean_sales_ts(sample_sales), type = "line", interactive = TRUE),
    class = "rlang_warning"
  )
  expect_s3_class(p, "ggplot")
})
