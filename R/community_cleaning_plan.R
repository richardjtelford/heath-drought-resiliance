

# clean community data


clean_comm <- function(comm0, spp_names, calluna_cover, meta0){
# Cleaning species names (when species list are clean, check it with a proper taxonomy list)

comm = comm0 |>
  #keep only resilience data
  mutate(
    year = as.numeric(year),
    experiment = tolower(experiment)) |> #fix case error
  filter(experiment == "resilience") |>
  # remove cover  == 0
  filter(cover > 0) |>

  #fix of species names
  mutate(species = case_when(
    species == "cladonia_sp" ~ "Cladonia_sp",
    species == "llex_aquifolium" ~ "Ilex_aquifolium",
    TRUE ~ species
  )) |>
  left_join(spp_names, by= 'species') |>
  mutate(species = coalesce(correct_name, species)) |>  #jumps to second if first in NA, keep first if it is not NA
  select(-correct_name) |>
  #remove species == NA - various calluna elements (seedlings, rootshoot etc)
  filter(species != "NA") |>
  clean_plot_ids(plot_list = meta0) |> 


  #### fixing species ####
  mutate(species = case_when(
    site == 'BUO' & species %in% c('Agrostis_canina', 'Agrostis_capillaris', 'Agrostis_sp') ~ 'Agrostis_capillaris',
    site == 'NOV' & species == 'Agrostis_canina' ~ 'Agrostis_vinealis',
    site == 'GOL' & species == 'Lycopodium_annitonum' ~ 'Lycopodum_clavatum',
    site == 'GOL' & species %in% c('Polygala_vulgaris', 'Polygala_sp') ~ 'Polygala_serpyllifolia',
    site == 'GOL' & species == 'Diphasiastrum_sp' ~ 'Diphasiastrum_alpinum',
    (site == 'NOV' & species == 'Viola_sp') | (site == 'BUO' & species %in% c('Viola_canina1', 'Viola_sp', 'Viola_riviniana')) ~ 'Viola_canina',
    site == 'TOR' & species == 'Carex_binervis' ~ 'Carex_vaginata',
    site == 'BUO' & species %in% c('Betula_nana', 'Betula_sp') ~ 'Betula_pubescens',
    site == 'BUO' & species == 'Vicia_sp' ~ 'Vicia_cracca',
    site == 'BUO' & species %in% c('Polygala_serpyllifolia', 'Polygala_sp') ~ 'Polygala_vulgaris',
    site == 'BUO' & species %in% c('Galium_saxatile', 'Galium_sp') ~ 'Galium_boreale',
    site %in% c('YST', 'LYG') & species == 'Polygala_vulgaris' ~ 'Polygala_serpyllifolia',
    site == 'LYG' & plot == '7.1.' & species == 'Carex_nigra' ~ 'Carex_echinata',
    site == 'LYG' & plot == '1.1.' & species == 'Festuca_sp' & year == '2017' ~ 'Festuca_vivipara',
    site == 'LYG' & plot %in% c('1.2.', '3.1.') & species == 'Festuca_rubra' & year == '2019' ~ 'Festuca_vivipara',
    site == 'LYG'  & species == 'Luzula_sylvatica' & year == '2019' ~ 'Luzula_pilosa',
    site == 'LYG' & plot == '2.2.' & species == 'Hylocomium_splendens' & year == '2018' ~ 'Hypnum_sp',
    site == 'LYG' & plot == '2.3.' & species == 'Luzula_pilosa' & year == '2018' ~ 'Luzula_multiflora',
    site == 'LYG' & plot == '3.2.' & species == 'Carex_sp' & year == '2019' ~ 'Carex_pilulifera',
    site == 'LYG' & plot %in% c('4.3.', '6.2.', '6.3.') & species == 'Carex_sp' & year == '2016' ~ 'Carex_pilulifera',
    site == 'LYG' & plot == '3.2.' & species == 'Lotus_corniculatus' & year == '2018' ~ 'Galium_saxatile',
    site == 'LYG' & plot == '3.3.' & species == 'Luzula_multiflora_cf' & year == '2019' ~ 'Luzula_multiflora',
    site == 'LYG' & plot == '5.1.' & species == 'Carex_binervis' & year == '2018' ~ 'Eriophorum_angustifolium',
    site == 'LYG' & plot == '5.2.' & species == 'Carex_sp' & year == '2017' ~ 'Eriophorum_angustifolium',
    site == 'LYG' & plot == '7.3.' & species == 'Eriophorum_vaginatum' & year %in% c('2017', '2016') ~ 'Eriophorum_angustifolium',
    site == 'LYG' & plot == '9.2.' & species == 'Eriophorum_vaginatum' & year == '2019' ~ 'Eriophorum_angustifolium',
    site == 'LYG' & plot == '5.2.' & species == 'Carex_flava' & year == '2016' ~ 'Carex_pilulifera',
    site == 'LYG' & plot %in% c('5.2.', '5.3.') & species == 'Carex_nigra' & year == '2016' ~ 'Carex_panicea',
    site == 'LYG' & plot %in% c('5.3.', '6.1.') & species == 'Carex_echinata' & year == '2016' ~ 'Carex_pilulifera',
    site == 'LYG' & plot %in% c('5.2.', '5.3.') & species == 'Juncus_sp' & year == '2016' ~ 'Juncus_squarrosus',
    site == 'LYG' & plot == '7.1.' & species %in% c('Carex_binervis', 'Carex_pilulifera') & year == '2017' ~ 'Carex_echinata',
    site == 'GOL' & plot %in% c('19.4.', '19.7.', '19.8.') &  species == 'Carex_sp' & year == '2016' ~ 'Carex_pilulifera',
    site == 'GOL' & plot == '19.8.' &  species == 'Festuca_sp' & year == '2017' ~ 'Festuca_rubra',
    site == 'NOV' & plot == '20.2.' &  species == '??' & year == '2018' ~ 'Unknown_herb',
    site == 'NOV' & plot == '20.4.' &  species == 'Rhytidiadelphus_squarrosus' & year == '2017' ~ 'Rhytidiadelphus_loreus',
    site == 'NOV' & plot == '20.9.' &  species == 'Anthoxanthum_odoratum' & year == '2017' ~ 'Agrostis_capillaris',
    site == 'NOV' & plot == '20.9.' &  species == 'Carex_sp' & year %in% c('2016', '2018') ~ 'Carex_pilulifera',
    site == 'NOV' & plot == '20.9.' &  species == 'Rhytidiadelphus_loreus' & year == '2018' ~ 'Rhytidiadelphus_squarrosus',
    TRUE ~ species )) |>
  ## fix NA group identified by drawings on field-sheet
  mutate(
    species = case_when(
      site == "YST" & species == "Unknown" & group == "NA" & cover == 10 ~ "Moss_sp",
      site == "YST" & species == "Unknown" & group == "NA" & cover == 1 ~ "Unknown_herb",
      TRUE ~ species
    ),
    group = case_when(
      site == "YST" & species == "Unknown" & group == "NA" & cover == 10 ~ "Bryophyte",
      site == "YST" & species == "Unknown" & group == "NA" & cover == 1 ~ "Forb",
      TRUE ~ group
    )
  ) |>
  #combine any species that need merging
  group_by(site, plot, year, species, group) |>
  summarise(cover = sum(cover, na.rm = TRUE)) |>
  #remove taxa with unknown group (1% cover)
  filter(group != "NA") |> 
  # rescue corrected calluna data
  left_join(calluna_cover, by = join_by(site == lokalitet, year, plot)) |> 
  mutate(cover = if_else(species == "Calluna_vulgaris", true =  vital_korr + damaged_korr, false = cover)) |> 
  select(- treatment, -(dead_org:vital_korr))

  comm
}
