ordination_plan = drake_plan(
  comm_fat = comm %>% 
    left_join(
      select(meta0, plot, treatment),
      by = "plot") %>%
    left_join(site_data, by = c("site" = "Site")) %>% 
    ungroup() %>%
    filter(!is.na(site)) %>% 
    filter(!(site == "BER" & year == "2017" & plot == "11.")) %>% 
    select(-group) %>% 
    pivot_wider(names_from = "species", values_from = "cover", values_fill = 0) %>% 
    arrange(year),
  
  comm_nmds = comm_fat %>%
    select(-(site:code)) %>% 
    metaMDS(),
  
  comm_fort = fortify(comm_nmds) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_fat %>% select(code, plot, year, treatment)),
  
  nmds_plot = ggplot(comm_fort, aes(x = NMDS1, y = NMDS2,  linetype = treatment ,colour = code, shape = treatment)) +
    geom_point(aes(size = year == min(year))) + 
    geom_path(aes(group = plot)) +
    scale_size_discrete(range = c(1, 2)) +
    labs(colour = "Site", size = "First year", linetype = "Treatment", shape = "Treatment")
  
)