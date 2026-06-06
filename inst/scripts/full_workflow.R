#salesTS - workflow
#skrypt dziala na danych przykladowych wbudowanych w pakiet, aby uzyc
#prawdziwych danych, nalezy ustawic USE_SAMPLE na FALSE a nastepnie poprawic
#sciezki w kroku 0

library(salesTS)
suppressPackageStartupMessages(library(dplyr))

#0: konfiguracja sciezek
USE_SAMPLE <- TRUE
train_path <- "data-raw/train.csv"
stores_path <- "data-raw/stores.csv"
holidays_path <- "data-raw/holidays_events.csv"

#1: wczytanie z filtrem
if (USE_SAMPLE) {
  #wersja na danych wbudowanych: laczymy sprzedaz z metadanymi sklepow.
  data <- sample_sales |>
    left_join(sample_stores, by = "store_nbr")
} else {
  data <- load_sales_data(
    train_path, stores_path, holidays_path,
    stores = 1:10,
    families = c("BEVERAGES", "PRODUCE", "MEATS")
  )
}

#2: walidacja jakosci danych
walidacja <- validate_sales_ts(data)
print(walidacja)
print(summary(walidacja))

#3: czyszczenie
dane_czyste <- clean_sales_ts(
  data,
  missing = "interpolate",
  dedupe = "sum",
  sort = TRUE
)
print(attr(dane_czyste, "cleaning_log"))

#4: metryki sprzedazy
metryki <- compute_sales_metrics(dane_czyste, group_by = c("store_nbr", "family"))
print(head(metryki, 10))

#5 wykresy
wykres_linia <- plot_sales_trends(dane_czyste, type = "line", group_by = "family")
wykres_heatmapa <- plot_sales_trends(dane_czyste, type = "heatmap")
wykres_promo <- plot_sales_trends(dane_czyste, type = "promo", group_by = "family")
#print(wykres_linia); print(wykres_heatmapa); print(wykres_promo)

#6: podsumowanie managerskie
podsumowanie <- create_management_summary(dane_czyste, period_days = 30)
print(podsumowanie)

#7: wiele analiz naraz
#do zastosowania zestaw funkcji do wybranego typu sklepuw w rozbiciu na sklepy
#typ wybierac z danych aby filtr cos zwrocil

typ_filtr <- if (USE_SAMPLE) sort(unique(dane_czyste$type))[1] else "A"
wyniki <- sales_ts_logic(
  dane_czyste,
  .funs = list(
    metryki = function(d) compute_sales_metrics(d, group_by = "family"),
    liczba_wierszy = nrow
  ),
  filters = list(type = typ_filtr),
  group_by = "store_nbr"
)
str(wyniki, max.level = 2)

#8: prognoza ARIMA v prophet
prognoza <- create_prognosis(
  dane_czyste,
  series = list(store_nbr = 1, family = "BEVERAGES"),
  models = c("arima", "prophet"),
  horizon = 30,
  frequency = "day",
  backtest = TRUE
)
print(prognoza)
#print(plot(prognoza))

message("Pipeline zakonczony pomyslnie.")