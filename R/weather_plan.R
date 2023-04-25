load_weather <- function(weather_data) {
  # weather data
  weather <- excel_sheets(weather_data)[-1] |>
    set_names() |>
    map(read_excel, path = weather_data) |> 
    list_rbind(names_to = "variable") |>
    mutate(Datetime = dmy_hms(Datetime)) |>
    pivot_longer(-c(variable, Datetime), names_to = "code", values_to = "value") |>
    mutate(code = factor(toupper(code), levels = LETTERS[7:1]))
  weather
}

# weather plot
make_weather_plot <- function(weather, site_colours) {
  RH_plot <- weather |>
    filter(variable == "RH") |>
    ggplot(aes(x = Datetime, y = value, colour = code)) +
    labs(y = "Relative Humidity %", colour = "Site")

  temp_plot <- weather |>
    filter(variable == "Air_temp") |>
    ggplot(aes(x = Datetime, y = value, colour = code)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    labs(y = "Temperature °C", colour = "Site")


  RH_plot / temp_plot + patchwork::plot_layout(guides = "collect") &
    geom_line() &
    site_colours &
    theme(axis.title.x = element_blank())
}


load_climate <- function(climate_data) {
  sheets <- excel_sheets(climate_data)[-1]
  climate <- sheets |>
    set_names(c("Precipitation", "Temperature", "RH")) |>
    map(~ {
      message("variable ", .x)
      climate_excel <- read_excel(climate_data, sheet = .x, skip = 1, na = c("", "-"))

      map(1 + 0:6 * 3, ~ select(climate_excel, .x:(.x + 2))) |>
        map(set_names, c("station", "date", "value")) |>
        list_rbind() |>
        filter(!is.na(station)) |>
        separate(date, into = c("month", "year"), sep = "\\.") |>
        filter(month %in% c("01", "02")) |>
        mutate(
          year = as.numeric(year),
          month = recode(month, `01` = "January", `02` = "February"),
          month = factor(month, levels = c("January", "February")),
          code = recode(station,
            "Hitra - Sandstad Ii" = "d",
            "Smøla - Moldstad" = "d",
            "Rørvik Lufthavn" = "e",
            "Otterøy" = "e",
            "Slåtterøy Fyr" = "a",
            "Ytre Solund" = "a",
            "Svinøy Fyr" = "c",
            "Fiskåbygd" = "c",
            "Tjøtta" = "g",
            "Vega - Vallsjø" = "f",
            "Ytterøyane Fyr" = "b",
            "Hildre" = "b"
          ),
          code = factor(toupper(code), levels = LETTERS[7:1])
        ) |>
        group_by(code) |>
        mutate(range = paste(min(year), "-", max(year)))
    }) |>
    list_rbind(names_to = "variable")
  climate
} # end climate

make_climate_plot <- function(climate, site_fill) {
  plot_climate <- function(data, binwidth = 1, xlab = "", ylim) {
    ggplot(data = data, aes(x = value, fill = code)) +
      geom_histogram(show.legend = FALSE, binwidth = binwidth, boundary = 0) +
      geom_segment(
        data = data |> filter(year == 2014),
        mapping = aes(x = value, xend = value, y = 4, yend = 0),
        colour = "black", arrow = arrow(length = unit(1.5, "mm"))
      ) +
      site_fill +
      geom_text(data = data |> slice(1, .by = code), aes(label = range), 
                x = min(data$value, na.rm = TRUE),
                y = 9, size = 2.7, hjust = 0.05) +
      facet_grid(rows = vars(code), cols = vars(variable), scales = "free_x") +
      scale_y_continuous(breaks = c(0, 5, 10), limits = ylim) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(x = xlab, y = "Number of years") 
  }
  
  ylim <- c(0, 11)
  P_plot <- climate |>
    filter(month == "January", variable == "Precipitation") |>
    plot_climate(binwidth = 10, xlab = "Precipitation mm", ylim = ylim) +
    theme(
      strip.background.y = element_blank(),
      strip.text.y = element_blank(),
      plot.margin = margin(5.5, 2, 5.5, 5.5, "pt")
    )

  T_plot <- climate |>
    filter(month == "January", variable == "Temperature") |>
    plot_climate(binwidth = 0.4, xlab = "Temperature °C", ylim = ylim) +
    theme(
      axis.title.y = element_blank(),
      strip.background.y = element_blank(),
      strip.text.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(5.5, 2, 5.5, 2, "pt")
    )

  RH_plot <- climate |>
    filter(month == "January", variable == "RH") |>
    mutate(variable = "Relative Humidity") |>
    plot_climate(binwidth = 1, xlab = "Relative Humidity %", ylim = ylim) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(5.5, 5.5, 5.5, 2, "pt")
    )

  P_plot + T_plot + RH_plot
}
