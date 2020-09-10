figures_plan <- drake_plan(
  # map
  map  = { 
    mp = map_data("worldHires", xlim = c(0, 40), ylim = c(55, 75))
    ggplot(site_data, aes(x = Longitude, y = Latitude, label = code)) +
      geom_map(data = mp, map = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey70", colour = "grey40") +
      geom_point() +
      geom_text_repel(direction  = "x") +
      coord_quickmap(xlim = c(NA, 28), ylim = c(58.2, 71)) +
      labs(x = "°E", y = "°N")
  },
  
  
  # calluna cover
  calluna_cover_plot = calluna_cover %>% 
    select(-ends_with("org")) %>% 
    pivot_longer(ends_with("korr")) %>% 
    mutate(
      name = factor(name, 
                    levels = c("dead_korr", "damaged_korr", "vital_korr"), 
                    labels = c("Dead", "Damaged", "Live")), 
      value = as.numeric(value)) %>% 
    ggplot(aes(x = factor(year), y = value, fill = treatment, colour = treatment)) + 
   # geom_boxplot() + 
    stat_summary() + 
    labs(x = "Year", y = "Cover %", fill = "Treatment") +
    scale_fill_discrete(limits = c("B", "C"), labels = c("Burnt", "Control")) +
    facet_grid(name ~ lokalitet),
  
  calluna_cover_plot2 = calluna_cover %>% 
    select(-ends_with("org")) %>% 
    pivot_longer(ends_with("korr")) %>% 
    mutate(
      name = factor(name, 
                    levels = c("dead_korr", "damaged_korr", "vital_korr"), 
                    labels = c("Dead", "Damaged", "Live")), 
      value = as.numeric(value)) %>% 
    group_by(lokalitet, year, name, treatment) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>% 
    ggplot(aes(x = factor(year), y = value, fill = name)) + 
    geom_col() + 
    labs(x = "Year", y = "Cover %", fill = "Treatment") +
    facet_grid(lokalitet ~ treatment, labeller = labeller(treatment = c(B = "Burnt", C = "Control")))
  
  
)
