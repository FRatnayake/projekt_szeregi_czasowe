# Shared fixtures for the test suite. All tests run on the packaged sample data
# (and the inst/extdata CSV fixtures) -- no external files are required.

sample_joined <- function() {
  dplyr::left_join(salesTS::sample_sales, salesTS::sample_stores, by = "store_nbr")
}

extdata_path <- function(file) {
  system.file("extdata", file, package = "salesTS")
}

# A clean, holiday-enriched dataset for tests that need is_holiday.
loaded_with_holidays <- function() {
  load_sales_data(
    extdata_path("train_sample.csv"),
    extdata_path("stores.csv"),
    extdata_path("holidays_events.csv")
  )
}
