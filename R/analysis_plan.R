 
  #seedling latitude
make_seedling_model <- function(seedlings, meta0, site_data) {
  seed_data <- seedlings |>
    full_join(
      crossing(year = unique(seedlings$year), meta0),
      by = c("site", "plot", "year")
    ) |>
    left_join(site_data,  by = c("site" = "Site")) |> 
    select(site, treatment, plot, year, seedlings_total, Latitude) |>
    mutate(seedlings_total = replace_na(seedlings_total, 0)) |> 
    filter(treatment %in% c("Burnt", "Unburnt")) 
  
  mod_treatment <-  glmer.nb(seedlings_total ~ treatment + (1|site/plot), data = seed_data)
   mod_latitude <- glmer.nb(seedlings_total ~ I(Latitude - 63.5) + (1|site/plot), data = seed_data, subset = treatment == "Burnt")
   list(treatment = mod_treatment, latitude = mod_latitude)
}

make_calluna_models <- function(calluna_cover) {
  
  # recovery in unburnt treatment
  mod_unburnt_recovery = calluna_cover |>
    filter(treatment == "Unburnt") |> 
    lmer(vital_korr ~ I(year-2016) + (1|lokalitet/plot), data = _)


  # recovery in burnt treatment
  mod_burnt_recovery <- calluna_cover |> 
    filter(treatment == "Burnt", year > 2016) |> 
    lmer(vital_korr ~ I(year - 2016) + (1|lokalitet/plot), data = _)

  # return results
  list(
    mod_unburnt_recovery = mod_unburnt_recovery, 
    mod_burnt_recovery = mod_burnt_recovery
    )
}

