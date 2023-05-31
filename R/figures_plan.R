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
make_calluna_cover_plot <- function(calluna_cover, site_data) {
  calluna_cover |>
    select(-ends_with("org")) |>
    left_join(site_data, by = c("lokalitet" = "Site")) |>
    pivot_longer(ends_with("korr")) |>
    mutate(
      name = factor(name,
        levels = c("dead_korr", "damaged_korr", "vital_korr"),
        labels = c("Dead", "Damaged", "Healthy")
      ),
      value = as.numeric(value)
    ) |>
    group_by(code, year, name, treatment) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = factor(year), y = value, fill = name)) +
    geom_col() +
    geom_vline(aes(xintercept = 1.5, colour = I(colour), linetype = I(linetype)), 
               data = tibble(treatment = factor("Burnt", levels = c("Unburnt", "Burnt")), colour = c("black", "red"), linetype = c("solid", "dashed"))) +
    scale_fill_manual(values = c("grey30", "grey70", "#4DAF4A")) +
    scale_y_continuous(breaks = scales::extended_breaks(n = 4)) +
    labs(x = "Year", y = "Cover %", fill = "Calluna status") +
    facet_grid(rows = vars(code), cols = vars(treatment)) +
    theme(
      strip.text.y = element_text(angle = 0),
      legend.position = "bottom"
    )
}

# community cover
make_community_group_cover_plot <- function(comm, meta0, site_data) {
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
    geom_vline(aes(xintercept = 1.5, colour = I(colour), linetype = I(linetype)), 
               data = tibble(treatment = factor("Burnt", levels = c("Unburnt", "Burnt")), colour = c("black", "red"), linetype = c("solid", "dashed"))) +
    scale_fill_brewer(palette = "Dark2") +
    facet_grid(rows = vars(code), cols = vars(treatment)) +
    labs(x = "Year", y = "Cover %", fill = "Functional Group") +
    theme(strip.text.y = element_text(angle = 0))
}

# seedling plot
make_seedling_plot <- function(seedlings, meta0, site_data, site_colours) {
  seedlings |>
    left_join(meta0, by = c("site", "plot")) |>
    left_join(site_data, by = c("site" = "Site")) |>
    filter(treatment == "Burnt") |>
    ggplot(aes(x = factor(year), y = seedlings_total, colour = code)) +
    ggbeeswarm::geom_beeswarm(show.legend = FALSE, dodge.width = 0.7) +
    labs(x = "Year", y = "Number of Calluna seedlings") +
    site_colours +
    theme(strip.text.y = element_text(angle = 0))
}

