
make_damage_model_data <- function(calluna_cover, site_data, env0){
  calluna_cover |>
    mutate(
      prop_dead = dead_korr / (dead_korr + damaged_korr + vital_korr),
      prop_damaged = damaged_korr / (dead_korr + damaged_korr + vital_korr),
      prop_healthy = vital_korr/ (dead_korr + damaged_korr + vital_korr)
    ) |>
  left_join(env0, by = "plot") |>
    left_join(site_data, by = c("lokalitet" = "Site")) |>
    filter(year == 2016) |>
    mutate(north = cos(Aspect * pi /180),
           meanPeatDepth = (Torvdjupne1+Torvdjupne2+Torvdjupne3 + Torvdjupne4+Torvdjupne5)/5
           ) |>
    select(-ends_with("korr"), -ends_with("org"), -matches("\\d$")) |>
    pivot_longer(starts_with("prop"), names_to = "vitality", values_to = "value") |>
    mutate(vitality = factor(vitality, levels = c("prop_dead", "prop_damaged", "prop_healthy")))
}

make_dead_damaged_plots <- function(damage_model_data, site_colours){
  dead_depth_plot = ggplot(damage_model_data, aes(x = meanPeatDepth, y = value, colour = code)) +
    geom_point(data = select(damage_model_data, -code), colour = "grey70") +
    geom_point(show.legend = FALSE) +
    facet_grid(code ~ vitality, labeller = labeller(vitality = c("prop_dead" = "Dead", "prop_damaged" = "Damaged", "prop_healthy" = "Healthy"))) +
    theme(strip.text.y = element_text(angle = 0)) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    labs(x = "Mean peat depth cm", y = expression(Proportion~italic(Calluna))) +
    site_colours

  dead_north_plot = dead_depth_plot +
    aes(x = north) +
    labs(x = "Aspect") +
    scale_x_continuous(breaks = seq(-1, 1, 0.5), labels = c("S", "SE/SW", "E/W", "NE/NW", "N"))

  dead_slope_plot = dead_depth_plot +
    aes(x = `Slope (°)`) +
    labs(x = "Slope (°)")
  
  list(dead_depth_plot = dead_depth_plot, dead_north_plot = dead_north_plot, dead_slope_plot = dead_slope_plot)
}