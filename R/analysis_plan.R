analysis_plan <- drake_plan(
  
  #seedling latitude 
  seedling_model = seedlings %>% 
    left_join(meta0, by = c("site", "plot")) %>% 
    left_join(site_data,  by = c("site" = "Site")) %>% 
    filter(treatment == "Burnt") %>% 
    glmer.nb(seedlings_total ~ I(Latitude -65) + I(year - 2016) + (1|site/plot), data = .),
  
  #proportion dead vs predictors
  mod_lat = damage_model_data %>% 
    lmer(value ~ Latitude + (1|lokalitet), data = ., subset = vitality == "prop_dead"),  
  mod_RH = damage_model_data %>% 
    lmer(value ~ RH + (1|lokalitet), data = ., subset = vitality == "prop_dead"), 
  mod_RH_depth = damage_model_data %>% 
    lmer(value ~ RH + meanPeatDepth + (1|lokalitet), data = ., subset = vitality == "prop_dead"), 
  mod_RH_north = damage_model_data %>% 
    lmer(value ~ RH + north + (1|lokalitet), data = ., subset = vitality == "prop_dead"), 

  # recovery in burnt treatment
  burnt_model_data = calluna_cover %>%
    mutate(
      prop_dead = dead_korr / (dead_korr + damaged_korr + vital_korr), 
      prop_damaged = damaged_korr / (dead_korr + damaged_korr + vital_korr), 
      prop_healthy = vital_korr/ (dead_korr + damaged_korr + vital_korr)
    ) %>% 
    left_join(env0, by = "plot") %>% 
    left_join(site_data, by = c("lokalitet" = "Site")) %>%
    mutate(north = cos(Aspect * pi /180), 
           meanPeatDepth = (Torvdjupne1+Torvdjupne2+Torvdjupne3 + Torvdjupne4+Torvdjupne5)/5
    ) %>% 
    filter(year > 2016, treatment == "Burnt") %>% 
    select(-ends_with("org"), -matches("\\d$")),
  
  burnt_year_mod = lmer(prop_healthy ~ I(year - 2016) + (1|lokalitet/plot), data = burnt_model_data),
  burnt_yearlat_mod = lmer(prop_healthy ~ Latitude + I(year - 2016) + (1|lokalitet/plot), data = burnt_model_data),
  burnt_yearxlat_mod = lmer(prop_healthy ~ Latitude * I(year - 2016) + (1|lokalitet/plot), data = burnt_model_data),
  burnt_yeardepth_mod = lmer(prop_healthy ~ Latitude + I(year - 2016) + meanPeatDepth + (1|lokalitet/plot),  data = burnt_model_data),
   burnt_yearnorth_mod = lmer(prop_healthy ~ Latitude + I(year - 2016) + north + (1|lokalitet/plot),  data = burnt_model_data),
  
    
)

#





#model recovery in burnt plots - all cover (except B L)
