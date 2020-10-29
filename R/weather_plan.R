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
      labs(y = "Relative Humidity %", colour = "Site") 
      
      
    RH_plot / temp_plot + patchwork::plot_layout(guides = "collect") &
      geom_line() &
      scale_colour_brewer(palette = "Dark2") &
      theme(axis.title.x = element_blank()) 
      
  },
  
  climate = {
    climate_excel <-read_excel("data/Mean RH_jan_feb.xlsx")
    climate <- map(1 + 0:6 * 3, ~select(climate_excel, .x:(.x + 2))) %>% 
      map_df(set_names, c("station", "date", "mean_RH")) %>% 
      filter(!is.na(station)) %>% 
      separate(date, into = c("month", "year"), sep = "\\.") %>% 
      mutate(
        across(c(month, year), as.numeric),
        month = recode(month, `1` = "January", `2` = "February"),
        month = factor(month, levels = c("January", "February")),
        code = recode(station, 
                      "Hitra - Sandstad Ii" = "d",   
                      "Rørvik Lufthavn" = "e",       
                      "Slåtterøy Fyr" = "a",         
                      "Svinøy Fyr" = "c",            
                      "Tjøtta" = "g",                 
                      "Vega - Vallsjø" = "f",         
                      "Ytterøyane Fyr" = "b"),
        code = factor(code, levels = letters[7:1])
      ) %>% 
    group_by(station) %>% 
    mutate(range = paste(min(year), "-", max(year)))
        
    climate
  },
  
  climate_plot = climate %>% 
    ggplot(aes(x = mean_RH, fill = code)) +
    geom_bar(width = 1, show.legend = FALSE) +
    #geom_vline(data = climate %>% filter(year == 2014), mapping = aes(xintercept = mean_RH), colour = "red") +
    geom_bar(data = climate %>% filter(year == 2014), width = 1, fill = "black") +
    scale_fill_brewer(palette = "Dark2") +
    geom_text(data = climate %>% filter(month == "January") %>% slice(1), aes(label = range), x = 55, y = 6, size = 2.7) +
    facet_grid(code ~ month) +
    labs(x = "Mean relative humidity %", y = "Number of years")
  
)
