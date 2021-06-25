weather_plan <- drake_plan(
  #weather data
  weather = {
    weather_file <- "data/Resiliens_climate_jan_feb.xlsx" 
    weather <- excel_sheets(weather_file)[-1] %>% 
      set_names() %>% 
      map_df(read_excel, path = weather_file, .id = "variable") %>% 
      mutate(Datetime = dmy_hms(Datetime)) %>% 
      pivot_longer(-c(variable, Datetime), names_to = "code", values_to = "value") %>% 
      mutate(code = factor(tolower(code), levels = letters[7:1]))
    weather
  },
  
  # weather plot  
  weather_plot =  {
    RH_plot <- weather %>% 
      filter(variable == "RH") %>% 
      ggplot(aes(x = Datetime, y = value, colour = code)) +
      labs(y = "Relative Humidity %", colour = "Site") 
  
    temp_plot <- weather %>% 
      filter(variable == "Air_temp") %>% 
      ggplot(aes(x = Datetime, y = value, colour = code)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      labs(y = "Temperature °C", colour = "Site") 
      
      
    RH_plot / temp_plot + patchwork::plot_layout(guides = "collect") &
      geom_line() &
      scale_colour_brewer(palette = "Dark2") &
      theme(axis.title.x = element_blank()) 
      
  },
  
  climate = {
    climate_file <- "data/Nedbør.xlsx"
    sheets <- excel_sheets(climate_file)[-1]
    climate <- sheets %>% 
      set_names(c("Precipitation", "Temperature", "RH" )) %>% 
      map_dfr(~ {
        message("variable ", .x)
        climate_excel <-read_excel(climate_file, sheet = .x, skip = 1, na = c("", "-"))
     
        map(1 + 0:6 * 3, ~select(climate_excel, .x:(.x + 2))) %>% 
          map_df(set_names, c("station", "date", "value")) %>% 
          filter(!is.na(station)) %>% 
          separate(date, into = c("month", "year"), sep = "\\.") %>% 
          filter(month %in% c("01", "02")) %>% 
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
                          "Hildre" = "b"),
            code = factor(code, levels = letters[7:1])
          ) %>% 
        group_by(code) %>% 
        mutate(range = paste(min(year), "-", max(year)))
    }, .id = "variable")    
    climate
  },#end climate

 climate_plot = {
   plot_climate <- function(data, binwidth = 1, xlab = "") {
     ggplot(data = data, aes(x = value, fill = code)) +
       geom_histogram( show.legend = FALSE, binwidth = binwidth, boundary = 0) +
       geom_segment(data = data %>% filter(year == 2014),
                    mapping = aes(x = value, xend = value, y = 4, yend  = 0),
                    colour = "black", arrow = arrow(length = unit(1.5, "mm"))) +
       scale_fill_brewer(palette = "Dark2") +
       geom_text(data = data %>% filter(month == "January") %>% slice(1), aes(label = range), x = min(data$value, na.rm = TRUE), y = 9, size = 2.7, hjust = 0.05) +
       facet_grid(code ~ variable, scales = "free_x") +
       theme(strip.text.y = element_text(angle = 0)) +
       labs(x = xlab, y = "Number of years")
   }
   
   P_plot <- climate %>% 
   filter(month == "January", variable == "Precipitation") %>% 
     plot_climate(binwidth = 10, xlab = "Precipitation mm") +
     theme(strip.background.y = element_blank(),
           strip.text.y = element_blank(),
           plot.margin = margin(5.5, 2, 5.5, 5.5, "pt")
     )
  
   T_plot <- climate %>% 
     filter(month == "January", variable == "Temperature") %>% 
     plot_climate(binwidth = 0.4, xlab = "Temperature °C") +
     theme(axis.title.y = element_blank(),
       strip.background.y = element_blank(),
       strip.text.y = element_blank(), 
       axis.text.y = element_blank(), 
       axis.ticks.y = element_blank(),
       plot.margin = margin(5.5, 2, 5.5, 2, "pt")
     )
   
   RH_plot <- climate %>% 
     filter(month == "January", variable == "RH") %>%
     mutate(variable = "Relative Humidity") %>% 
     plot_climate(binwidth = 1, xlab = "Relative Humidity %") +
     theme(axis.title.y = element_blank(), 
           axis.text.y = element_blank(), 
           axis.ticks.y = element_blank(),
           plot.margin = margin(5.5, 5.5, 5.5, 2, "pt")
           )
 
   P_plot + T_plot + RH_plot & ylim(0, 11)
})
