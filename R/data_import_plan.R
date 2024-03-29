## import and process data

# import meta data
load_metadata <- function(meta_download) {
  read_xlsx(meta_download) |>
    mutate(
      treatment = recode(treatment, "C" = "Unburnt", "B" = "Burnt"),
      # fix wrong treatments
      treatment = case_when(
        plot == "22.0." ~ "Unburnt",
        plot == "22.9." ~ "Burnt",
        TRUE ~ treatment
      )) |> 
    filter(treatment %in% c("Burnt", "Unburnt")) |> 
    mutate(
      treatment = factor(treatment),
      treatment = fct_relevel(treatment, "Unburnt")
    )
}

# import environment
load_environment <- function(environment_download) {env0 <- read_xlsx(environment_download, na = "na") |>
  rename(plot = Plott...3) |>
  # add trailing . to plot names
  mutate(plot = if_else(!str_detect(plot, "\\.$"), true = paste0(plot, "."), false = plot))
}

# import community
load_community <- function(community_download) {
  read_xlsx(community_download,
  sheet = "frequency",
  col_types = "text",
  na = "NA"
  ) |>
  mutate(
    cover = recode(cover, ".+" = "1"),
    cover = as.numeric(cover)
  )
}

load_spp_names <- function(community_download){ 
  read_xlsx(community_download, sheet = "corr_names") |>
  filter(!(is.na(correct_name))) |>
  distinct(species, .keep_all = TRUE) |> # Remove duplicates
  mutate(
    group = case_when(
      correct_name == "Moss_sp" ~ "Bryophyte",
      correct_name == "Kalmia_procumbens" ~ "Ericales",
      TRUE ~ group
    ),
    correct_name = if_else(correct_name == "Hyperikum_maculatum", "Hypericum_maculatum", correct_name)
  )
}

# import calluna cover
load_calluna_cover <- function(calluna_cover_download){
  read_xlsx(calluna_cover_download, sheet = "Ferdigstilling", na = "na") |>
  mutate(
    treatment = recode(treatment, "C" = "Unburnt", "B" = "Burnt"),

  ) |>
  mutate(
    plot = if_else(!str_detect(plot, "\\.$"), true = paste0(plot, "."), false = plot),
    treatment = case_when(
      plot == "25.5." ~ "Burnt",
      plot == "25.0." ~ "Unburnt",
      .default = treatment
    ), 
    treatment = factor(treatment),
    treatment = fct_relevel(treatment, "Unburnt")
    )
}

load_seedlings <- function(calluna_cover_download, meta0){
  read_xlsx(calluna_cover_download, sheet = "seedling", na = "na") |> 
    clean_plot_ids(plot_list = meta0)
}

load_all_covers <- function(calluna_cover_download){
  read_xlsx(calluna_cover_download, sheet = "cover", na = "NA") |>
  select(-pellets, -metode, -...43, -...44) |> # has problems but unused in analysis
  # fix plot codes
  mutate(
    # add dot at end
    plot = str_replace(plot, "^(\\d{1,2}\\.\\d{1,2})$", "\\1."),
    # fix remaining problems
    plot = case_when(
      lokalitet == "TOR" & year == 2018 & plot == 14 ~ "24.14.",
      lokalitet == "TOR" & year == 2018 & plot == 16 ~ "24.16.",
      lokalitet == "ROS" & year == 2019 & plot == "22.10." ~ "22.0.",
      lokalitet == "SKO" & year == 2019 & plot == "25.10." ~ "25.0.",
      TRUE ~ plot
    )
  ) |>
  # fix non-numeric values in data
  mutate(
    across(-plot, ~ str_replace(.x, ",", ".")), # change "," to "."
    across(-plot, ~ str_replace(.x, "\\.$", "")), # fix stray "." after number
    across(-plot, ~ str_replace(.x, "^+$", "1")), # change "+" to "1"
    across(-plot, ~ if_else(.x == "N", NA_character_, .x)), # change "N" to NA
  ) |>
  # convert to numeric
  # will lose ~30 non-numeric values at the moment
  mutate(across(max_height1:cover_bryophytes, as.numeric)) |>
  bind(.x, mutate(.x, mean_max_height = rowMeans(select(.x, starts_with("max_height")), na.rm = TRUE)))
}

make_site_data <- function(comm) {
  read_delim("Site\tSite name	Mean annual precipitation (mm)	Mean January temperature (C)	Mean July temperature (C)	Biogeographic section (Moen, 1998)	Latitude	Longitude
BER	Bergsnova	1535			O2	64.841056	10.848461
BUO	Store Buøya	1132			O1	65.83677	12.224506
GOL	Golta	2080			O2	60.220656	5.001823
HAV	Haverøya	1136			O1	64.779	11.2193
LYG	Lygra	3209			O2	60.70084	5.092566
NOV	Novelandet	2673			O2	61.807887	4.922439
ROS	Rossvolløya	1136			O2	63.304347	8.007803
SKO	Skotsvær	1309			O1	65.79602	12.219299
TOR	Torsøya	1131			O2	65.68843	12.067688
YST	Ytstevika	2560			O2	62.359774	5.519645", delim = "\t") |>
    arrange(desc(Latitude)) |>
    semi_join(comm, by = c("Site" = "site")) |>
    mutate(
      code = factor(LETTERS[n():1]),
      code = fct_rev(code)
    )
}
