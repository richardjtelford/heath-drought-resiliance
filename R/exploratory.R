damage_model_data = calluna_cover %>%
  mutate(prop_dead = dead_korr / (dead_korr + damaged_korr + vital_korr)) %>% 
  left_join(env0, by = "plot") %>% 
  left_join(site_data, by = c("lokalitet" = "Site")) %>%
  filter(year == 2016) 

mod_lat = lm(prop_dead ~ Latitude, damage_model_data)  
summary(mod_lat)
plot(mod_lat)
  
ggplot(damage_model_data, aes(x = Latitude, y = prop_dead)) + 
  geom_point() +
  geom_smooth()

ggplot(damage_model_data, aes(x = Aspect, y = prop_dead, colour = code)) + 
  geom_point() +
  geom_smooth(aes(group = 1))

ggplot(damage_model_data, aes(y = Aspect, x = code, colour = code, size = prop_dead)) + 
  geom_jitter(height = 0) +
  scale_y_continuous(breaks = seq(0, 360, 90), limits = c(0, 360))

ggplot(damage_model_data, aes(x = code, y = (Torvdjupne1+Torvdjupne2+Torvdjupne3 + Torvdjupne4+Torvdjupne5)/5, colour = code, size = prop_dead)) + 
  geom_jitter(height = 0) 
