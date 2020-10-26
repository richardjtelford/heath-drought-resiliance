damage_plan <- drake_plan(
  damage_model_data = calluna_cover %>%
    mutate(prop_dead = dead_korr / (dead_korr + damaged_korr + vital_korr)) %>% 
    left_join(env0, by = "plot") %>% 
    left_join(site_data, by = c("lokalitet" = "Site")) %>%
    filter(year == 2016) %>% 
    mutate(north = cos(Aspect * pi /180), 
           meanPeatDepth = (Torvdjupne1+Torvdjupne2+Torvdjupne3 + Torvdjupne4+Torvdjupne5)/5
           ),
  
  damage_lat_plot = ggplot(damage_model_data, aes(x = Latitude, y = prop_dead)) + 
    geom_point() +
    geom_smooth(),
  
  damage_north_plot = ggplot(damage_model_data, aes(x = north, y = prop_dead, colour = code)) + 
    geom_point(data = select(damage_model_data, -code), colour = "grey70") +
    geom_point() +
    facet_wrap(~ code) +
    labs(x = "North, cos(Aspect)", y = "Proportion Calluna dead"),
  
  damage_depth_plot = ggplot(damage_model_data, aes(x = meanPeatDepth, colour = code, y = prop_dead)) +
    geom_point(data = select(damage_model_data, -code), colour = "grey70") +
    geom_point() +
    facet_wrap(~ code) +
    labs(x = "Mean peat depth cm", y = "Proportion Calluna dead")
)
