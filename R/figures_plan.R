figures_plan <- drake_plan(
  # map
  map  = { 
    mp = map_data("worldHires", xlim = c(0, 40), ylim = c(55, 75))
    ggplot(site_data, aes(x = Longitude, y = Latitude, label = code)) +
      geom_map(data = mp, map = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey75", colour = "grey50") +
      geom_point() +
      geom_label_repel(direction  = "x", force = 2, point.padding = 0.2) +
      coord_quickmap(xlim = c(NA, 27), ylim = c(58.2, 71)) +
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
                    labels = c("Dead", "Damaged", "Healthy")), 
      value = as.numeric(value)) %>% 
    group_by(code, year, name, treatment) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>% 
    ggplot(aes(x = factor(year), y = value, fill = name)) + 
    geom_col() + 
    scale_fill_brewer(palette = "Set1") + 
    labs(x = "Year", y = "Cover %", fill = "Calluna status") +
    facet_grid(code ~ treatment) +
    theme(strip.text.y = element_text(angle = 0)),
  
  #community cover
  community_group_cover = comm %>% 
    inner_join(meta0, by = c("site", "plot")) %>% 
    left_join(site_data, by = c("site" = "Site")) %>% 
    group_by(code, treatment, year, group) %>% 
    summarise(cover = mean(cover)) %>%
    ggplot(aes(x = year, y = cover, fill = group)) +
    geom_col() + 
    scale_fill_brewer(palette = "Dark2") +
    facet_grid(code ~ treatment) +
    labs(x = "Year", y = "Cover %", fill = "Functional Group") +
    theme(strip.text.y = element_text(angle = 0)),
  
  #seedling plot
  seedling_plot = seedlings %>% 
    left_join(meta0, by = c("site", "plot")) %>% 
    left_join(site_data,  by = c("site" = "Site")) %>% 
    filter(treatment == "Burnt") %>% 
    ggplot(aes(x = factor(year), y = seedlings_total, fill = code)) +
    geom_boxplot(show.legend = FALSE) + 
    facet_grid(code ~ .) +
    labs(x = "Year", y = "Number of Calluna seedlings") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_fill_brewer(palette = "Dark2") +
    theme(strip.text.y = element_text(angle = 0)) 
  
  
)

  
