analysis_plan <- drake_plan(
  
)

loadd()
library(lme4)

library(lmerTest)
mod_lat <- damage_model_data %>% 
  lmer(value ~ Latitude + (1|lokalitet), data = ., subset = vitality == "prop_dead")
summary(mod_lat)

mod_lat_peat <- damage_model_data %>% 
  lmer(value ~ scale(Latitude) *scale( north) + (1|lokalitet), data = ., subset = vitality == "prop_dead")
summary(mod_lat_peat)


damage_model_data %>% 
  lmer(value ~ north + (1|lokalitet), data = ., subset = vitality == "prop_dead") %>% summary()


#reorder cover plot Ericales on top, B & L on bottom
# model against humidy
#model recovery in burnt plots - healthy and all cover (except B L)