ordination_plan = drake_plan(
  comm_fat = comm %>% 
    inner_join(
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
  
  nmds_plot = ggplot(comm_fort, aes(x = NMDS1, y = NMDS2, colour = code)) +
    geom_point(aes(size = year == min(year))) + 
    geom_path(aes(group = plot)) +
    scale_size_discrete(range = c(1, 2)) +
    scale_colour_brewer(palette = "Dark2") +
    scale_y_continuous(breaks = c(-1, 0, 1)) +
    labs(colour = "Site", size = "First year") +
    coord_equal() +
    facet_wrap(~ treatment),
  
  # species in nmds plot
  
  spp_summ = comm %>% 
    group_by(species) %>% 
    filter(cover > 0) %>% 
    summarise(mx = max(cover), n = n()) %>% 
    filter(mx > 20, n > 15) %>% 
    mutate(name_species = str_replace(species, "^([A-Z][a-z]{2}).*_([a-z]{2,3}).*", "\\1_\\2")),
    
  nmds_species_plot = fortify(comm_nmds) %>% 
    filter(Score == "species") %>% 
    left_join(spp_names, by = c("Label" = "species")) %>% 
    left_join(spp_summ, by = c("Label" = "species")) %>% 
    ggplot(aes(x = NMDS1, y = NMDS2, label = name_species, colour = group)) +
    geom_point(shape = "+", size = 2) +
    scale_colour_brewer(palette = "Dark2") +
    geom_text_repel(show.legend = FALSE), 
  
  #### PCA ####
  PCA = comm_fat %>%
    select(-(site:code)) %>% 
    sqrt() %>% 
    rda(),
  
  PCA_fort = fortify(PCA) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_fat %>% select(code, plot, year, treatment)),
  
  PCA_plot = ggplot(PCA_fort, aes(x = PC1, y = PC2, colour = code)) +
    geom_point(aes(size = year == min(year))) + 
    geom_path(aes(group = plot)) +
    scale_size_discrete(range = c(1, 2)) +
    scale_colour_brewer(palette = "Dark2") +
    scale_y_continuous(breaks = c(-1, 0, 1)) +
    labs(colour = "Site", size = "First year") +
    coord_equal() +
    facet_wrap(~ treatment)
  
)
