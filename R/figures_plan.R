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
  
  
  # Calluna cover
  calluna_cover_plot = calluna_cover %>% 
    select(-ends_with("org")) %>% 
    left_join(site_data, by = c("lokalitet" = "Site") ) %>% 
    pivot_longer(ends_with("korr")) %>% 
    mutate(
      name = factor(name, 
                    levels = c("dead_korr", "damaged_korr", "vital_korr"), 
                    labels = c("Dead", "Damaged", "Live")), 
      value = as.numeric(value)) %>% 
    group_by(code, year, name, treatment) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(treatment = factor(treatment, levels = c("C", "B"))) %>% 
    ggplot(aes(x = factor(year), y = value, fill = name)) + 
    geom_col() + 
    scale_fill_brewer(palette = "Set1") + 
    labs(x = "Year", y = "Cover %", fill = "Treatment") +
    facet_grid(code ~ treatment, labeller = labeller(treatment = c(B = "Burnt", C = "Control"))) +
    theme(strip.text.y = element_text(angle = 0)),
  
  community_group_cover = comm %>% 
    left_join(meta0, by = c("site", "plot")) %>% 
    left_join(site_data, by = c("site" = "Site")) %>% 
    group_by(code, treatment, year, group) %>% 
    summarise(cover = mean(cover)) %>%
    ggplot(aes(x = year, y = cover, fill = group)) +
    geom_col() + 
    scale_fill_brewer(palette = "Dark2") +
    facet_grid(code ~ treatment) +
    theme(strip.text.y = element_text(angle = 0))
)
