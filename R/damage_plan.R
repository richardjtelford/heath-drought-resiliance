damage_plan <- drake_plan(
  damage_model_data = calluna_cover %>%
    mutate(
      prop_dead = dead_korr / (dead_korr + damaged_korr + vital_korr), 
      prop_damaged = damaged_korr / (dead_korr + damaged_korr + vital_korr), 
      prop_dead_damaged = prop_dead + prop_damaged) %>% 
  left_join(env0, by = "plot") %>% 
    left_join(site_data, by = c("lokalitet" = "Site")) %>%
    filter(year == 2016) %>% 
    mutate(north = cos(Aspect * pi /180), 
           meanPeatDepth = (Torvdjupne1+Torvdjupne2+Torvdjupne3 + Torvdjupne4+Torvdjupne5)/5
           ),
 
  dead_damage_lat_plot = damage_model_data %>% 
    select(code, Latitude, year, prop_dead, prop_damaged) %>%
    pivot_longer(starts_with("prop"), names_to = "name", values_to = "value") %>% 
    {ggplot(., aes(x = Latitude, y = value, colour = code, shape = name)) + 
        geom_point(show.legend = FALSE, position = position_dodge(width = 0.06)) +
        geom_label_repel(aes(x = Latitude, y = 0, colour = code, label = code,), 
                     data = distinct(., code, Latitude), 
                     direction = "x", nudge_x = 0.1, show.legend = FALSE, inherit.aes = FALSE) +
        scale_colour_brewer(palette = "Dark2") +
        scale_shape_manual(limits = c("prop_dead", "prop_damaged"), values = c(16, 1)) +
        new_scale_color() +
        geom_smooth(aes(colour = name, group = name, linetype = name), se = FALSE, show.legend = FALSE) +
        scale_colour_manual(limits = c("prop_dead", "prop_damaged"), values = c("black", "grey60")) +
        scale_linetype_manual(limits = c("prop_dead", "prop_damaged"), values = c("solid", "dashed")) +
        
        labs(x = "Latitude °N", y = "Proportion Calluna Dead or Damaged")
  }  
  
   
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
