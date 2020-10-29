weather_plan <- drake_plan(
  #weather data
  weather = {
    climate_file <- "data/Resiliens_climate_jan_feb.xlsx" 
    weather <- excel_sheets(climate_file)[-1] %>% 
      set_names() %>% 
      map_df(read_excel, path = climate_file, .id = "variable") %>% 
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
      labs(y = "Relative Humidity %", colour = "Site") 
      
      
    RH_plot / temp_plot + patchwork::plot_layout(guides = "collect") &
      geom_line() &
      scale_colour_brewer(palette = "Dark2") &
      theme(axis.title.x = element_blank()) 
      
  }
)