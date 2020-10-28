damage_plan <- drake_plan(
  damage_model_data = calluna_cover %>%
    mutate(
      prop_dead = dead_korr / (dead_korr + damaged_korr + vital_korr), 
      prop_dead_damaged = (dead_korr + damaged_korr) / (dead_korr + damaged_korr + vital_korr)) %>% 
    left_join(env0, by = "plot") %>% 
    left_join(site_data, by = c("lokalitet" = "Site")) %>%
    filter(year == 2016) %>% 
    mutate(north = cos(Aspect * pi /180), 
           meanPeatDepth = (Torvdjupne1+Torvdjupne2+Torvdjupne3 + Torvdjupne4+Torvdjupne5)/5
           ),
  
  dead_lat_plot = ggplot(damage_model_data, aes(x = Latitude, y = prop_dead, colour = code, label = code)) + 
    geom_point(show.legend = FALSE) +
    geom_smooth(aes(group = 1), se = FALSE, show.legend = FALSE) +
    geom_label_repel(aes(y = 0), 
                     data = distinct(damage_model_data, code, Latitude), 
                     direction = "x", nudge_x = 0.1, show.legend = FALSE) +
    scale_colour_brewer(palette = "Dark2") +
    labs(x = "Latitude °N", y = "Proportion Calluna Dead"),
  
  dead_north_plot = ggplot(damage_model_data, aes(x = north, y = prop_dead, colour = code)) + 
    geom_point(data = select(damage_model_data, -code), colour = "grey70") +
    geom_point(show.legend = FALSE) +
    facet_wrap(~ code) +
    labs(x = "North, cos(Aspect °)", y = "Proportion Calluna dead") +
    scale_colour_brewer(palette = "Dark2"),
  
  dead_depth_plot = dead_north_plot +
    aes(x = meanPeatDepth) +
    labs(x = "Mean peat depth cm"),
  
  dead_slope_plot = dead_north_plot +
    aes(x = `Slope (°)`) +
    labs(x = "Slope (°)"),
  
  ## dead + damaged
  damage_lat_plot = dead_lat_plot + 
    aes(y = prop_dead_damaged) +
    labs(y = "Proportion Calluna Dead & Damaged"),
  
  damage_north_plot = dead_north_plot + 
    aes(y = prop_dead_damaged) +
    labs(y = "Proportion Calluna Dead & Damaged"),
  
  damage_depth_plot = dead_depth_plot + 
    aes(y = prop_dead_damaged) +
    labs(y = "Proportion Calluna Dead & Damaged"),
  
  damage_slope_plot = dead_slope_plot +
    aes(y = prop_dead_damaged) +
    labs(y = "Proportion Calluna Dead & Damaged"),
  
)
