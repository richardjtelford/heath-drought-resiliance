make_species_list <- function(spp_names, comm, file = "Species_abbreviations.csv" ) {

spp_names |> 
  distinct(group, species = correct_name) |> 
  semi_join(comm, by = join_by(species == species)) |> 
  mutate(
    group = recode(group, "Fern" = "Fern and Forb", "Forb" = "Fern and Forb", "Wood" = "Woody"),
    group = factor(group, levels = c("Ericales", "Woody", "Graminoid", "Fern and Forb", "Bryophyte", "Lichen")),
    species = str_replace(species, " ", "_"),
    Abbreviation = str_replace(species, "^([A-Z][a-z]{2}).*_([a-z]{2,3}).*", "\\1 \\2"),
    Abbreviation = str_to_title(Abbreviation),
    Abbreviation = str_remove(Abbreviation, " ")
  ) |> 
  mutate(species = str_replace(species, "_", " ")) |> 
  arrange(species) |> 

  select(Species = species, Abbreviation, "Functional Group" = group) |> 
  readr::write_csv(file = file)
  file

}