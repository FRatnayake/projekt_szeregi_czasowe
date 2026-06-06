#' Visualise retail sales trends
#'
#' A single entry point for the most useful sales visualisations: time series with
#' a moving average and promotion/holiday markers, an STL decomposition, a
#' weekly/yearly seasonality heatmap, a promotion-effect plot and a holiday-effect
#' plot.
#'
#' @param data A sales tibble with at least `date`, `sales`, `onpromotion`.
#' @param group_by Optional character vector of columns used to colour/facet the
#'   `"line"` plot (e.g. `"family"`).
#' @param type One of `"line"`, `"decomposition"`, `"heatmap"`, `"promo"`,
#'   `"holiday"`.
#' @param interactive Logical; if `TRUE` and the suggested package
#'   \pkg{plotly} is installed, the plot is returned as an interactive
#'   \pkg{plotly} object. Otherwise a warning is emitted and a static
#'   \pkg{ggplot2} object is returned.
#'
#' @return A \pkg{ggplot2} object (or a \pkg{plotly} object when
#'   `interactive = TRUE` and \pkg{plotly} is available).
#'
#' @details
#' * `"line"` overlays a 7-day moving average, marks promotion days with a rug and
#'   (when `is_holiday` is present) holidays with dashed vertical lines.
#' * `"decomposition"` aggregates everything to one daily series, regularises it
#'   and runs [stats::stl()]; it errors if the series is shorter than two seasonal
#'   cycles.
#' * `"heatmap"` shows mean sales over day-of-week x ISO week-of-year.
#' * `"promo"` compares the sales distribution on promotion vs non-promotion days.
#' * `"holiday"` requires `is_holiday` and compares holiday vs working days.
#'
#' @examples
#' # Assign to a variable (the plot renders only when printed).
#' p_line <- plot_sales_trends(sample_sales, type = "line")
#' p_heat <- plot_sales_trends(sample_sales, type = "heatmap")
#' p_promo <- plot_sales_trends(sample_sales, type = "promo", group_by = "family")
#' \dontrun{
#' print(p_line)
#' }
#'
#' @export
plot_sales_trends <- function(data,
                              group_by = NULL,
                              type = c("line", "decomposition", "heatmap", "promo", "holiday"),
                              interactive = FALSE) {
  check_columns(data, c("date", "sales", "onpromotion"))
  type <- rlang::arg_match(type)
  if (!is.null(group_by) && !is.character(group_by)) {
    cli::cli_abort("Argument {.arg group_by} musi by\u0107 wektorem znakowym albo NULL.")
  }
  if (!is.null(group_by)) {
    check_columns(data, group_by, arg = "group_by")
  }

  p <- switch(type,
    line = plot_line(data, group_by),
    decomposition = plot_decomposition(data),
    heatmap = plot_heatmap(data),
    promo = plot_promo(data, group_by),
    holiday = plot_holiday(data, group_by)
  )

  maybe_interactive(p, interactive)
}

# Return a plotly object when requested and available; otherwise degrade.
maybe_interactive <- function(p, interactive) {
  if (!isTRUE(interactive)) {
    return(p)
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_warn(
      "Pakiet {.pkg plotly} nie jest zainstalowany - zwracam statyczny wykres ggplot2."
    )
    return(p)
  }
  plotly::ggplotly(p)
}

plot_line <- function(data, group_by) {
  grp <- group_by %||% character(0)
  df <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("date", grp)))) |>
    dplyr::summarise(
      sales = sum(.data$sales, na.rm = TRUE),
      promo = mean(.data$onpromotion > 0, na.rm = TRUE),
      .groups = "drop"
    )

  if (length(grp) > 0) {
    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
      dplyr::arrange(.data$date, .by_group = TRUE) |>
      dplyr::mutate(ma = zoo::rollmean(.data$sales, k = 7, fill = NA, align = "right")) |>
      dplyr::ungroup()
    df$grupa <- interaction(df[grp], drop = TRUE, sep = " | ")
    aes_main <- ggplot2::aes(x = .data$date, y = .data$sales, colour = .data$grupa)
    aes_ma <- ggplot2::aes(x = .data$date, y = .data$ma, colour = .data$grupa)
  } else {
    df <- df |>
      dplyr::arrange(.data$date) |>
      dplyr::mutate(ma = zoo::rollmean(.data$sales, k = 7, fill = NA, align = "right"))
    aes_main <- ggplot2::aes(x = .data$date, y = .data$sales)
    aes_ma <- ggplot2::aes(x = .data$date, y = .data$ma)
  }

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(aes_main, alpha = 0.45) +
    ggplot2::geom_line(aes_ma, linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_rug(
      data = dplyr::filter(df, .data$promo > 0),
      mapping = ggplot2::aes(x = .data$date),
      sides = "b", alpha = 0.3, colour = "darkorange"
    ) +
    ggplot2::labs(
      title = "Sprzeda\u017c w czasie ze \u015bredni\u0105 krocz\u0105c\u0105 (7 dni)",
      subtitle = "Pomara\u0144czowe znaczniki na dole: dni z promocj\u0105",
      x = "Data", y = "Sprzeda\u017c", colour = "Grupa"
    ) +
    ggplot2::theme_minimal()

  if ("is_holiday" %in% names(data)) {
    hol_dates <- data |>
      dplyr::filter(.data$is_holiday) |>
      dplyr::distinct(.data$date) |>
      dplyr::pull(.data$date)
    if (length(hol_dates) > 0) {
      p <- p + ggplot2::geom_vline(
        xintercept = hol_dates, linetype = "dashed",
        colour = "grey60", alpha = 0.5
      )
    }
  }
  p
}

plot_decomposition <- function(data) {
  ser <- data |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(sales = sum(.data$sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$date)

  full <- tibble::tibble(date = seq(min(ser$date), max(ser$date), by = "day"))
  ser <- full |>
    dplyr::left_join(ser, by = "date") |>
    dplyr::mutate(sales = as.numeric(zoo::na.approx(.data$sales, na.rm = FALSE, rule = 2)))

  freq <- 7L
  if (nrow(ser) < 2L * freq) {
    cli::cli_abort(c(
      "Seria jest za kr\u00f3tka do dekompozycji STL.",
      "x" = "Potrzeba co najmniej {.val {2L * freq}} obserwacji, a jest {.val {nrow(ser)}}.",
      "i" = "Wybierz d\u0142u\u017cszy zakres dat lub inn\u0105 seri\u0119."
    ))
  }

  ts_obj <- stats::ts(ser$sales, frequency = freq)
  dec <- stats::stl(ts_obj, s.window = "periodic")
  comp <- as.data.frame(dec$time.series)

  long <- tibble::tibble(
    date = ser$date,
    Obserwacja = ser$sales,
    Trend = comp$trend,
    Sezonowosc = comp$seasonal,
    Reszta = comp$remainder
  ) |>
    tidyr::pivot_longer(
      -"date",
      names_to = "skladowa", values_to = "wartosc"
    ) |>
    dplyr::mutate(skladowa = factor(
      .data$skladowa,
      levels = c("Obserwacja", "Trend", "Sezonowosc", "Reszta")
    ))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$date, y = .data$wartosc)) +
    ggplot2::geom_line(colour = "steelblue", na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(.data$skladowa), ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = "Dekompozycja STL szeregu sprzeda\u017cy",
      x = "Data", y = NULL
    ) +
    ggplot2::theme_minimal()
}

plot_heatmap <- function(data) {
  wday_labels <- c("Pon", "Wt", "Sr", "Czw", "Pt", "Sob", "Niedz")
  df <- data |>
    dplyr::mutate(
      dzien = factor(
        lubridate::wday(.data$date, week_start = 1),
        levels = 1:7, labels = wday_labels
      ),
      tydzien = lubridate::isoweek(.data$date)
    ) |>
    dplyr::group_by(.data$dzien, .data$tydzien) |>
    dplyr::summarise(srednia = mean(.data$sales, na.rm = TRUE), .groups = "drop")

  ggplot2::ggplot(df, ggplot2::aes(x = .data$tydzien, y = .data$dzien, fill = .data$srednia)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(
      title = "Sezonowo\u015b\u0107: dzie\u0144 tygodnia x tydzie\u0144 roku",
      x = "Tydzie\u0144 roku (ISO)", y = "Dzie\u0144 tygodnia", fill = "\u015arednia\nsprzeda\u017c"
    ) +
    ggplot2::theme_minimal()
}

plot_promo <- function(data, group_by) {
  df <- data |>
    dplyr::mutate(
      promocja = factor(
        ifelse(.data$onpromotion > 0, "Promocja", "Bez promocji"),
        levels = c("Bez promocji", "Promocja")
      )
    )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$promocja, y = .data$sales, fill = .data$promocja)) +
    ggplot2::geom_violin(alpha = 0.5, na.rm = TRUE) +
    ggplot2::geom_boxplot(width = 0.15, outlier.alpha = 0.2, na.rm = TRUE) +
    ggplot2::labs(
      title = "Rozk\u0142ad sprzeda\u017cy: dni z promocj\u0105 vs bez",
      x = NULL, y = "Sprzeda\u017c", fill = NULL
    ) +
    ggplot2::theme_minimal()

  facet <- intersect(group_by %||% character(0), c("family", "store_nbr", "type"))
  if (length(facet) > 0) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet[1]]]), scales = "free_y")
  }
  p
}

plot_holiday <- function(data, group_by) {
  if (!"is_holiday" %in% names(data)) {
    cli::cli_abort(c(
      "Wykres \u015bwi\u0105teczny wymaga kolumny {.field is_holiday}.",
      "i" = "Wczytaj dane z {.arg holidays_path} w {.fn load_sales_data}."
    ))
  }
  df <- data |>
    dplyr::mutate(
      dzien = factor(
        ifelse(.data$is_holiday, "\u015awi\u0119to", "Dzie\u0144 roboczy"),
        levels = c("Dzie\u0144 roboczy", "\u015awi\u0119to")
      )
    )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$dzien, y = .data$sales, fill = .data$dzien)) +
    ggplot2::geom_boxplot(outlier.alpha = 0.2, na.rm = TRUE) +
    ggplot2::labs(
      title = "Wp\u0142yw \u015bwi\u0105t na sprzeda\u017c",
      x = NULL, y = "Sprzeda\u017c", fill = NULL
    ) +
    ggplot2::theme_minimal()

  facet <- intersect(group_by %||% character(0), c("family", "store_nbr", "type"))
  if (length(facet) > 0) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet[1]]]), scales = "free_y")
  }
  p
}
