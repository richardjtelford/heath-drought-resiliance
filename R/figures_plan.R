# figures

# map
make_map <- function(site_data) {
  mp <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf", country = c("Norway", "Sweden", "Finland"), continent = "Europe")
  box <- c(xmin = 0, xmax = 27, ymin = 56, ymax = 72)
  mp <- sf::st_crop(mp, sf::st_bbox(box))

  ggplot(mp, aes(x = Longitude, y = Latitude, label = code)) +
    geom_sf(fill = "grey80", colour = "grey70", inherit.aes = FALSE) +
    ggspatial::geom_spatial_point(data = site_data, crs = 4326) +
    ggspatial::geom_spatial_text_repel(data = site_data, hjust = 1.2, direction = "x", force = 2, point.padding = 0.2, crs = 4326) +
    coord_sf(xlim = c(-350000, 500000), ylim = c(-740000, 680000), expand = FALSE, crs = "+proj=laea +lon_0=10 +lat_0=64.5 +datum=WGS84 +units=m +no_defs") +
    ggspatial::annotation_scale(location = "tl") +
    theme(axis.title = element_blank())
}


# Calluna cover
make_calluna_cover_plot <- function(calluna_cover, comm, meta0, site_data) {
  dummy <- tibble(treatment = factor("Burnt", levels = c("Unburnt", "Burnt")), colour = "Spring 2017")
  # find non-calluna cover
  non_calluna <-   comm |>
    inner_join(meta0, by = c("site", "plot")) |>
    left_join(site_data, by = c("site" = "Site")) |> 
    filter(
      species != "Calluna_vulgaris", 
      !group %in% c("Lichen", "Bryophyte")) |> 
    group_by(code, plot, year, treatment) |> 
    summarise(cover = sum(cover), .groups = "drop", status = factor("Other vascular"))
    
  
 calluna_cover2 <- calluna_cover |>
    select(-ends_with("org")) |>
    left_join(site_data, by = c("lokalitet" = "Site")) |>
    pivot_longer(ends_with("korr"), names_to = "status", values_to = "cover") |>
    mutate(
      status = factor(status,
        levels = c("non_calluna", "dead_korr", "damaged_korr", "vital_korr"),
        labels = c("Other vascular", "Dead", "Damaged", "Healthy")
      ),
      cover = as.numeric(cover)
    ) |>
    group_by(code, year, status, treatment) |> 
   bind_rows(non_calluna)
 
  bind_rows(Plot = calluna_cover2,
            `Site mean` = calluna_cover2 |> 
    summarise(cover = mean(cover, na.rm = TRUE), plot = "1", .groups = "drop"),
    .id = "what") |>
    mutate(what = factor(what, levels = c("Site mean", "Plot"))) |> 
    group_by(code, treatment) |> 
    arrange(plot) |> 
    mutate(year2 = year + (as.numeric(as.factor(plot)) - 3.5)/7 ) |> 
    arrange(desc(status)) |> 

    ggplot(aes(x = year2, y = cover, fill = status, group = plot, alpha = what)) +
    geom_col(position = position_stack()) +
    geom_vline(aes(xintercept = 2016.5, colour = colour, linetype = colour), 
                 data = dummy) +
    scale_fill_manual(values = c("#4DAF4A", "grey30", "grey70", "#d075c7")) +
    scale_y_continuous(breaks = scales::extended_breaks(n = 4)) +
    scale_x_continuous(expand = c(0.02, 0), breaks = c(2015:2020)) +
   scale_colour_manual(values = c("Spring 2017" = "#FF0000")) +
   scale_linetype_manual(values = c("Spring 2017" = "dashed")) +
    scale_alpha_manual(values = c(Plot = 0.5, `Site mean` = 1)) +
   labs(x = "Year", y = "Cover %", fill = expression(italic(Calluna)~status), colour = "Burn", linetype = "Burn", alpha = "Scale") +
    facet_grid(rows = vars(code), cols = vars(treatment)) +
    theme(
      strip.text.y = element_text(angle = 0),
      legend.position = "bottom"
    ) +
   guides(fill = guide_legend(order = 1, title.position="top", title.hjust = 0.5, nrow = 2), 
          colour = guide_legend(order = 2, title.position="top", title.hjust = 0.5), 
          linetype = guide_legend(order = 2, title.position="top", title.hjust = 0.5),
          alpha = guide_legend(order = 3, title.position="top", title.hjust = 0.5, nrow = 2))

}

# community cover
make_community_group_cover_plot <- function(comm, meta0, site_data) {
  dummy <- tibble(treatment = factor("Burnt", levels = c("Unburnt", "Burnt")), colour = "Spring 2017")
  comm |>
    inner_join(meta0, by = c("site", "plot")) |>
    left_join(site_data, by = c("site" = "Site")) |>
    mutate(
      group = recode(group, "Fern" = "Fern and Forb", "Forb" = "Fern and Forb", "Wood" = "Woody"),
      group = factor(group, levels = c("Ericales", "Woody", "Graminoid", "Fern and Forb", "Bryophyte", "Lichen"))
    ) |>
    group_by(code, treatment, year, group, plot) |> 
    summarise(cover = sum(cover), .groups = "drop") |> 
    group_by(code, treatment, year, group) |>
    summarise(cover = mean(cover), .groups = "drop") |>
    ggplot(aes(x = as.factor(year), y = cover, fill = group)) +
    geom_col() +
    geom_vline(aes(xintercept = 1.5, colour = colour, linetype = colour), 
               data = dummy) +
    scale_fill_manual(values = c("#d075c7", "#1FAF7F", "#D95F02", "#7570B3", "#66A61E", "#E6AB02")) +                              
    scale_colour_manual(values = c("Spring 2017" = "#FF0000")) +
    scale_linetype_manual(values = c("Spring 2017" = "dashed")) +
    facet_grid(rows = vars(code), cols = vars(treatment)) +
    labs(x = "Year", y = "Cover %", fill = "Functional Group", colour = "Burn", linetype = "Burn") +
    theme(strip.text.y = element_text(angle = 0)) +
    guides(fill = guide_legend(order = 1, title.position = "top"), colour = guide_legend(order = 2, title.position = "top"), linetype = guide_legend(order = 2, title.position = "top"))

}

# seedling plot
make_seedling_plot <- function(seedlings, meta0, site_data, site_colours) {
  seedlings |>
    left_join(meta0, by = c("site", "plot")) |>
    left_join(site_data, by = c("site" = "Site")) |>
    filter(treatment == "Burnt") |>
    ggplot(aes(x = factor(year), y = seedlings_total, colour = code)) +
    ggbeeswarm::geom_beeswarm(show.legend = TRUE, dodge.width = 0.7) +
    labs(x = "Year", y = "Number of Calluna seedlings", colour = "Site") +
    site_colours +
    theme(strip.text.y = element_text(angle = 0))
}

