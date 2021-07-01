figures_plan <- drake_plan(
  
  #default site colours
  # site_colours = scale_colour_brewer(palette = "Dark2"),
  # site_fill = scale_fill_brewer(palette = "Dark2"),
  site_colours = scale_colour_viridis_d(option = "B", end = 0.9),
  site_fill = scale_fill_viridis_d(option = "B", end = 0.9),
  
  
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
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = factor(year), y = value, fill = name)) + 
    geom_col() + 
    scale_fill_manual(values = c("grey30", "grey70", "#4DAF4A")) + 
    scale_y_continuous(breaks = scales::extended_breaks(n = 4)) +
    labs(x = "Year", y = "Cover %", fill = "Calluna status") +
    facet_grid(code ~ treatment) +
    theme(strip.text.y = element_text(angle = 0), 
          legend.position = "bottom"),
  
  #community cover
  community_group_cover = comm %>% 
    inner_join(meta0, by = c("site", "plot")) %>% 
    left_join(site_data, by = c("site" = "Site")) %>% 
    mutate(
      group = recode(group, "Fern" = "Fern and Forb", "Forb" = "Fern and Forb"),
      group = factor(group, levels = c("Ericales", "Wood", "Graminoid", "Fern and Forb", "Bryophyte", "Lichen"))
      ) %>% 
    group_by(code, treatment, year, group) %>% 
    summarise(cover = mean(cover), .groups = "drop") %>%
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
    ggplot(aes(x = factor(year), y = seedlings_total, colour = code)) +
    ggbeeswarm::geom_beeswarm(show.legend = FALSE,  groupOnX=TRUE) + 
    facet_grid(code ~ .) +
    labs(x = "Year", y = "Number of Calluna seedlings") +
    site_colours +
    theme(strip.text.y = element_text(angle = 0)) 
  
  
)

  
