# last worked on 23.11.2019 #

library('ggplot2')
library('readxl')
library('tidyverse')
library('ggpubr')

################ CALCULATING RDPIs ##############################

plotID <- read_xlsx('Data/drought.plots.xlsx')

traits <- readRDS('cleandata/traits.rds', refhook = NULL) %>% 
  left_join(plotID, by = 'plot') %>% 
  mutate(art = paste(Genus, Species, sep = '_'),
         phase = factor(phase, levels = c("pioneer", "building", "mature")))

controls <- traits %>% 
  select(Year, plot, art, phase, treatment, plantheight, thickness, SLA, LDMC, ID) %>% 
  filter(treatment == '0')


rdpi <- controls %>% 
  left_join(traits, by = c('phase', 'Year', 'art')) %>% 
  filter(!(ID.x == ID.y)) %>% 
  mutate(SLA = abs(SLA.y-SLA.x),
         LDMC = abs(LDMC.y - LDMC.x),
         plantheight = abs(plantheight.y - plantheight.x),
         thickness = abs(thickness.y - thickness.x)) %>% 
  mutate(phase = factor(phase, levels = c('pioneer', 'building', 'mature'))) %>% 
  mutate(phase = fct_recode(phase, "Young" = 'pioneer', "Intermediate" = 'building', 'Old' = 'mature')) %>%  #treatment = as.numeric(treatment) 
  group_by(Year,  art, phase, treatment.y) %>%  
  summarise_at(.vars = c('plantheight', 'SLA', 'LDMC', 'thickness'), mean, na.rm = TRUE) 



#p <-
ggplot(rdpi, aes(treatment.y, log(SLA), fill = treatment.y)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "palegreen3", "50" = "snow2", "90" = "plum3"), name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
  #scale_fill_brewer(direction = -1, name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
  theme_classic() +
  theme(#panel.grid.major = element_blank(), 
    #panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.title=element_text(size=17), 
    legend.text=element_text(size=15),
    plot.title = element_text( size=40, hjust = 0.5),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y = element_text( size=18),
    axis.line = element_line(linetype = "solid"),
    axis.text.y = element_text(size = 18)
  ) +
  ylab('log Plasticity Distance LDMC (mg/g)') +
  xlab ('\nPost-fire phase') +
  labs(fill='Drought frequency') +
  stat_compare_means( label = "p.signif", method='t.test', ref.group = '0', size = 8) +
  facet_grid(Year~phase) +
  theme( strip.text.x = element_blank()) +
  facet_wrap(~Year)

tiff("Figures/poster_rdpi.tiff", units="in", width=9, height=6, res=300)
p
dev.off() 

# two problems: 
# 1) 7885 rows removed because of non-finite values
# 2) some crazy significance levels

# Using means instead



##### working line ###########

group_by(phase, treatment, art, plot, Year) %>% 
  summarise_at(.vars = c('plantheight', 'SLA', 'LDMC', 'thickness'), mean, na.rm=TRUE)
# NA phase for four rows because observations don't have plot

phase_control <- traits %>% 
  group_by(phase, treatment, art, plot, Year) %>% 
  filter (treatment == '0') %>% 
  summarise_at(.vars = c('plantheight', 'SLA', 'LDMC', 'thickness'), mean, na.rm=TRUE) %>% 
  rename (SLA_c = "SLA",
          LDMC_c = "LDMC",
          PH_c = "plantheight",
          TH_c = "thickness",
          control = "treatment",
          plot_c = "plot") 

plot <- traits %>% 
  group_by(treatment,plot, art, phase, Year) %>% 
  summarise_at(.vars = c('plantheight', 'SLA', 'LDMC', 'thickness'), mean, na.rm=TRUE)





#### fra Johns paper ####

# Pr = ||(H-T) / H |







plast2 <- plast %>% 
  mutate(SLA = abs(abs(SLA.y - SLA.x)/SLA.y),
         LDMC = abs(abs(LDMC.y - LDMC.x)/LDMC.y),
         PH = abs(abs(Plant_height.y - Plant_height.x)/Plant_height.y),
         TH = abs(abs(Thickness.y - Thickness.x)/Thickness.y)
  ) %>% 
  filter (treatment.x == '50' | treatment.x == '90')

#### UGLY ####
# pooling controls so plots don't compare with themselves  
#pioner

# 'home' population
con.pio.1.1. <- all_traits %>% 
  filter ( plot == '2.2.' | plot == '3.1.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

con.pio.2.2. <- all_traits %>% 
  filter ( plot == '1.1.' | plot == '3.1.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

con.pio.3.1. <- all_traits %>% 
  filter ( plot == '2.2.' | plot == '1.1.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

con.pio.4.3. <- all_traits %>% 
  filter ( plot == '5.1.' | plot == '6.2.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

con.pio.5.1. <- all_traits %>% 
  filter ( plot == '4.3.' | plot == '6.2.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

con.pio.6.2. <- all_traits %>% 
  filter ( plot == '5.1.' | plot == '4.3.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

con.pio.7.3. <- all_traits %>% 
  filter ( plot == '8.3.' | plot == '9.3.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

con.pio.8.3. <- all_traits %>% 
  filter ( plot == '7.3.' | plot == '9.3.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

con.pio.9.3. <- all_traits %>% 
  filter ( plot == '8.3.' | plot == '7.3.') %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

# population to test  
plot.1.1. <- all_traits %>% 
  filter ( plot == '1.1.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)  

plot.2.2. <- all_traits %>% 
  filter ( plot == '2.2.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

plot.3.1. <- all_traits %>% 
  filter ( plot == '3.1.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

plot.4.3. <- all_traits %>% 
  filter ( plot == '4.3.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

plot.5.1. <- all_traits %>% 
  filter ( plot == '5.1.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

plot.6.2. <- all_traits %>% 
  filter ( plot == '6.2.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

plot.7.3. <- all_traits %>% 
  filter ( plot == '7.3.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

plot.8.3. <- all_traits %>% 
  filter ( plot == '8.3.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

plot.9.3. <- all_traits %>% 
  filter ( plot == '9.3.') %>% 
  group_by(plot, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)


# join these to calculate RPDI
con.1.1. <- inner_join(plot.1.1., con.pio.1.1., by = 'art')
con.2.2. <- inner_join(plot.2.2., con.pio.2.2., by = 'art')
con.3.1. <- inner_join(plot.3.1., con.pio.3.1., by = 'art')
con.4.3. <- inner_join(plot.4.3., con.pio.4.3., by = 'art')
con.5.1. <- inner_join(plot.5.1., con.pio.5.1., by = 'art')
con.6.2. <- inner_join(plot.6.2., con.pio.6.2., by = 'art')
con.7.3. <- inner_join(plot.7.3., con.pio.7.3., by = 'art')
con.8.3. <- inner_join(plot.8.3., con.pio.8.3., by = 'art')
con.9.3. <- inner_join(plot.9.3., con.pio.9.3., by = 'art')

# Can probably repeat nine times, and then use rbind
con <- rbind(con.1.1.,
             con.2.2., 
             con.3.1., 
             con.4.3., 
             con.5.1., 
             con.6.2., 
             con.7.3.,
             con.8.3.,
             con.9.3.)

# Calculate RPDI (simplified)
controls <- con %>% 
  mutate(SLA = abs(abs(SLA.y - SLA.x)/SLA.y),
         LDMC = abs(abs(LDMC.y - LDMC.x)/LDMC.y),
         PH = abs(abs(Plant_height.y - Plant_height.x)/Plant_height.y),
         TH = abs(abs(Thickness.y - Thickness.x)/Thickness.y)
  )

# Gjere 50/90 og 0 df like så ein kan bruke rbind

# 50/90 df:
treat <- select(plast2, -"treatment.y") %>% 
  rename(treatment = treatment.x)

Lygra <- rbind(controls, treat) 

# PLOY

#p <-
ggplot(RDPI, aes(treatment, LDMC, fill=treatment)) +
  geom_boxplot() +
  scale_fill_brewer(palette ='PiYG') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  ) +
  xlab ('Drought treatment') +
  labs(fill='Drought treatment') +
  ylab('Relative distance plasticity index') +
  facet_wrap(~phase)

tiff("LDMC_plast_rett_kontroll.tiff", units="in", width=7, height=3, res=300)
p
dev.off() 
