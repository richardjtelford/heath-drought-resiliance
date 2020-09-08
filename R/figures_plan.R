figures_plan <- drake_plan(
  calluna_cover_plot = calluna_cover %>% 
    select(-ends_with("org")) %>% 
    pivot_longer(ends_with("korr")) %>% 
    mutate(
      name = factor(name, 
                    levels = c("dead_korr", "damaged_korr", "vital_korr"), 
                    labels = c("Dead", "Damaged", "Live")), 
      value = as.numeric(value)) %>% 
    ggplot(aes(x = factor(year), y = value, fill = treatment)) + 
    geom_boxplot() + 
    labs(x = "Year", y = "Cover %", fill = "Treatment") +
    scale_fill_discrete(limits = c("B", "C"), labels = c("Burnt", "Control")) +
    facet_grid(name ~ lokalitet)
)
