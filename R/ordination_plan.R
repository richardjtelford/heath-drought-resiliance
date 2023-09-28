# ordinations
make_wide_comm <- function(comm, meta0, site_data) {
  comm |>
    inner_join(
      select(meta0, plot, treatment),
      by = "plot"
    ) |>
    left_join(site_data, by = c("site" = "Site")) |>
    ungroup() |>
    filter(!is.na(site)) |>
    filter(!(site == "BER" & year == "2017" & plot == "11.")) |>
    select(-group) |>
    filter(n() > 2, .by = species) |> 
    pivot_wider(names_from = "species", values_from = "cover", values_fill = 0) |>
    arrange(year)
}


make_spp_summ <- function(comm){
  comm |>
  group_by(species) |>
  filter(cover > 0) |>
  summarise(mx = max(cover), n = n(), .groups = "drop") |>
  filter(mx > 20, n > 15) |>
  mutate(
    name_species = str_replace(species, "^([A-Z][a-z]{2}).*_([a-z]{2,3}).*", "\\1 \\2"),
    name_species = str_to_title(name_species),
    name_species = str_remove(name_species, " ")
    )
}

make_nmds_plots <- function(comm_wide, comm, spp_names, spp_summ, site_colours) {
  comm_nmds <- comm_wide |>
    select(-(site:code)) |>
    metaMDS()

  comm_fort <- fortify(comm_nmds) |>
    filter(Score == "sites") |>
    bind_cols(comm_wide |> select(code, plot, year, treatment))

  nmds_plot <- {
    site_means <- comm_fort |>
      group_by(treatment, code, year) |>
      summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2), .groups = "drop")

    ggplot(comm_fort, aes(x = NMDS1, y = NMDS2, colour = code)) +
      #  geom_point(aes(size = year == min(year)), alpha = 0.2) +
      # geom_path(aes(group = plot), alpha = 0.2) +
      geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
      geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
      geom_point(aes(size = year > min(year)), # , shape = treatment
        data = site_means
      ) +
      geom_path(data = site_means, aes(linetype = treatment)) +
      scale_size_discrete(range = c(2, 1), labels = c("First", "Subsequent")) +
      site_colours +
      scale_y_continuous(breaks = c(-0.5, 0, 0.5)) +
      labs(colour = "Site", size = "Year", linetype = "Treatment") +
      coord_equal() +
      guides(
        colour = guide_legend(title.position = "top", byrow = TRUE), 
        linetype = guide_legend( title.position = "top", ncol = 1), 
        size = guide_legend(title.position = "top", ncol = 1)) +
      theme(legend.position = "bottom")
    #  facet_wrap(facets = vars(treatment))
  }

  # species in nmds plot



  nmds_species_plot <- fortify(comm_nmds) |>
    filter(Score == "species") |>
    left_join(distinct(spp_names, correct_name, group), by = c("Label" = "correct_name")) |>
    left_join(spp_summ, by = c("Label" = "species")) |>
    mutate(
      group = recode(group, "Fern" = "Fern and Forb", "Forb" = "Fern and Forb", "Wood" = "Woody"),
      group = factor(group, levels = c("Ericales", "Woody", "Graminoid", "Fern and Forb", "Bryophyte", "Lichen"))
    ) |>
    ggplot(aes(x = NMDS1, y = NMDS2, label = name_species, colour = group)) +
    geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
    geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
    geom_point(shape = "+", size = 2) +
    scale_colour_manual(values = c("#d075c7", "#1FAF7F", "#D95F02", "#7570B3", "#66A61E", "#E6AB02")) +
    ggrepel::geom_text_repel(show.legend = FALSE, size = 3) +
    labs(colour = "Functional Group") +
    guides(colour = guide_legend(override.aes = list(size = 3), byrow = TRUE)) +
    theme(legend.position = "bottom")

  list(nmds_plot = nmds_plot, nmds_species_plot = nmds_species_plot)
}



#### PCA ####

make_pca_plots <- function(comm_wide, spp_names, spp_summ, site_colours) {
  
  DCA <-  comm_wide |>
    select(-(site:code)) |>
    sqrt() |>
    decorana()
  
  PCA <- comm_wide |>
    select(-(site:code)) |>
    sqrt() |>
    rda()

  PCA_fort <- fortify(PCA) |>
    filter(Score == "sites") |>
    bind_cols(comm_wide |> select(code, plot, year, treatment)) |>
    mutate(year2 = factor(year == min(year), levels = c("TRUE", "FALSE")))

  pca_plot <- ggplot(
    PCA_fort,
    aes(
      x = PC1,
      y = PC2,
      colour = code
    )
  ) +
    geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
    geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
    geom_point(aes(size = year2)) +
    geom_path(aes(group = plot)) +
    scale_size_discrete(range = c(2, 1), labels = c("2016", "2017-2019")) +
    site_colours +
    scale_y_continuous(breaks = c(-1, 0, 1)) +
    labs(colour = "Site", size = "Year") +
    coord_equal() +
    facet_wrap(facets = vars(treatment))

  pca_plot_b <- {
    site_means <- PCA_fort |>
      group_by(treatment, code, year, year2) |>
      summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")


    ggplot(
      PCA_fort,
      aes(
        x = PC1,
        y = PC2,
        colour = code
      )
    ) +
      geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
      geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
      geom_point(aes(size = year2), alpha = 0.15) +
      geom_path(aes(group = plot), alpha = 0.15) +
      geom_point(aes(size = year2), data = site_means) +
      geom_path(data = site_means) +
      scale_size_discrete(range = c(2, 1), labels = c("2016", "2017-2019")) +
      site_colours +
      scale_y_continuous(breaks = c(-1, 0, 1)) +
      labs(colour = "Site", size = "Year") +
      coord_equal() +
      facet_wrap(facets = vars(treatment))
  }

  pca_plot_c <- {
    site_means <- PCA_fort |>
      group_by(treatment, code, year, year2) |>
      summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")


    ggplot(
      PCA_fort,
      aes(
        x = PC1,
        y = PC2,
        colour = code,
        linetype = treatment
      )
    ) +
      geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
      geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
      #    geom_point(aes(size = year2), alpha = 0.2) +
      #    geom_path(aes(group = plot), alpha = 0.2) +
      geom_point(aes(size = year2), data = site_means) +
      geom_path(data = site_means) +
      scale_size_discrete(range = c(2, 1), labels = c("2016", "2017-2019")) +
      site_colours +
      scale_y_continuous(breaks = c(-1, 0, 1)) +
      labs(colour = "Site", size = "Year") +
      coord_equal() # +
    #   facet_wrap(facets = vars(treatment))
  }


  PCA13_plot <- pca_plot + aes(x = PC1, y = PC3)

  PCA_species_plot <- fortify(PCA) |>
    filter(Score == "species") |>
    left_join(distinct(spp_names, correct_name, group), by = c("Label" = "correct_name")) |>
    left_join(spp_summ, by = c("Label" = "species")) |>
    ggplot(aes(x = PC1, y = PC2, label = name_species, colour = group)) +
    geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
    geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
    geom_point(shape = "+", size = 2) +
    scale_colour_brewer(palette = "Dark2") +
    ggrepel::geom_text_repel(show.legend = FALSE)

  list(
    DCA = DCA,
    PCA = PCA,
    pca_plot = pca_plot,
    pca_plot_b = pca_plot_b,
    pca_plot_c = pca_plot_c,
    PCA13_plot = PCA13_plot,
    PCA_species_plot = PCA_species_plot
  )
}

#### make CA ####

make_ca_plots <- function(comm_wide, spp_names, spp_summ, site_colours) {
  
  CA <- comm_wide |>
    select(-(site:code)) |>
    sqrt() |>
    cca()
  
  CA_fort <- fortify(CA) |>
    filter(Score == "sites") |>
    bind_cols(comm_wide |> select(code, plot, year, treatment)) |>
    mutate(year2 = factor(year == min(year), levels = c("TRUE", "FALSE")))
  
  ca_plot <- ggplot(
    CA_fort,
    aes(
      x = CA1,
      y = CA2,
      colour = code
    )
  ) +
    geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
    geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
    geom_point(aes(size = year2)) +
    geom_path(aes(group = plot)) +
    scale_size_discrete(range = c(2, 1), labels = c("2016", "2017-2019")) +
    site_colours +
    scale_y_continuous(breaks = c(-1, 0, 1)) +
    labs(colour = "Site", size = "Year") +
    coord_equal() +
    facet_wrap(facets = vars(treatment))
  
  ca_plot_b <- {
    site_means <- CA_fort |>
      group_by(treatment, code, year, year2) |>
      summarise(CA1 = mean(CA1), CA2 = mean(CA2), .groups = "drop")
    
    
    ggplot(
      CA_fort,
      aes(
        x = CA1,
        y = CA2,
        colour = code
      )
    ) +
      geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
      geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
      geom_point(aes(size = year2), alpha = 0.15) +
      geom_path(aes(group = plot), alpha = 0.15) +
      geom_point(aes(size = year2), data = site_means) +
      geom_path(data = site_means) +
      scale_size_discrete(range = c(2, 1), labels = c("2016", "2017-2019")) +
      site_colours +
      scale_y_continuous(breaks = c(-1, 0, 1)) +
      labs(colour = "Site", size = "Year") +
      coord_equal() +
      facet_wrap(facets = vars(treatment))
  }
  
  ca_plot_c <- {
    site_means <- CA_fort |>
      group_by(treatment, code, year, year2) |>
      summarise(CA1 = mean(CA1), CA2 = mean(CA2), .groups = "drop")
    
    
    ggplot(
      CA_fort,
      aes(
        x = CA1,
        y = CA2,
        colour = code,
        linetype = treatment
      )
    ) +
      geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
      geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
      #    geom_point(aes(size = year2), alpha = 0.2) +
      #    geom_path(aes(group = plot), alpha = 0.2) +
      geom_point(aes(size = year2), data = site_means) +
      geom_path(data = site_means) +
      scale_size_discrete(range = c(2, 1), labels = c("2016", "2017-2019")) +
      site_colours +
      scale_y_continuous(breaks = c(-1, 0, 1)) +
      labs(colour = "Site", size = "Year") +
      coord_equal() # +
    # facet_wrap(facets = vars(treatment))
  }
  
  ca_species_plot <- fortify(CA) |>
    filter(Score == "species") |>
    left_join(distinct(spp_names, correct_name, group), by = c("Label" = "correct_name")) |>
    left_join(spp_summ, by = c("Label" = "species")) |>
    ggplot(aes(x = CA1, y = CA2, label = name_species, colour = group)) +
    geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) + 
    geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.2) +
    geom_point(shape = "+", size = 2) +
    scale_colour_brewer(palette = "Dark2") +
    ggrepel::geom_text_repel(show.legend = FALSE)
  
  list(
    CA = CA,
    ca_plot = ca_plot,
    ca_plot_b = ca_plot_b,
    ca_plot_c = ca_plot_c,
    ca_species_plot = ca_species_plot
  )
}
