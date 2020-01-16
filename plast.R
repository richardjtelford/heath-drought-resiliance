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
         phase = factor(phase, levels = c("pioneer", "building", "mature")),
         single_area = Area_cm2 / NumberLeavesScan)

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



p <- 
ggplot(rdpi, aes(treatment.y, log(LDMC), fill = treatment.y)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue2", "50" = "springgreen4", "90" = "orange2"), name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
  #scale_fill_manual(values = c("0" = "palegreen3", "50" = "snow2", "90" = "plum3"), name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
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
    axis.title.y = element_text( size=12),
    axis.line = element_line(linetype = "solid"),
    axis.text.y = element_text(size = 12)
  ) +
  ylab('log Plasticity Distance LDMC') +
  xlab ('\nPost-fire phase') +
  labs(fill='Drought frequency') +
  stat_compare_means( label = "p.signif", method='t.test', ref.group = '0', size = 3) +
  facet_grid(Year~phase) 

#+
 # theme( strip.text.x = element_blank()) +
#  facet_wrap(~Year)

tiff("Figures/LDMC_jan20.tiff", units="in", width=6, height=6, res=300)
p
dev.off() 

# two problems: 
# 1) 7885 rows removed because of non-finite values
# 2) some crazy significance levels

# Using means instead



##### working line ###########

dry <- traits %>% 
  filter ( Year == '2018') %>% 
  select(art, plantheight, thickness, LDMC, SLA, treatment, phase, plot, Wetmass, Drymass, Area_cm2, single_area) %>% 
  group_by (art, treatment, phase, plot) %>% 
  summarise_at(.vars = c('plantheight', 'SLA', 'LDMC', 'thickness', 'Wetmass', 'Drymass', 'Area_cm2', 'single_area'), mean, na.rm = TRUE) %>% 
  rename(plantheight18 = plantheight,
         thickness18 = thickness,
         LDMC18 = LDMC,
         SLA18 = SLA,
         Wetmass18 = Wetmass,
        Drymass18 = Drymass,
        Area18 = Area_cm2,
        single_area18 = single_area)

wet <- traits %>% 
  filter ( Year == '2019') %>% 
  select(art, plantheight, thickness, LDMC, SLA, treatment, phase, plot, Wetmass, Drymass, Area_cm2, single_area) %>% 
  group_by (art, treatment, phase, plot) %>% 
  summarise_at(.vars = c('plantheight', 'SLA', 'LDMC', 'thickness', 'Wetmass', 'Drymass', 'Area_cm2', 'single_area'), mean, na.rm = TRUE) %>% 
rename(plantheight19 = plantheight,
         thickness19 = thickness,
         LDMC19 = LDMC,
         SLA19 = SLA,
       Wetmass19 = Wetmass,
       Drymass19 = Drymass,
       Area19 = Area_cm2,
       single_area19 = single_area)

wet_dry <- wet %>% 
  inner_join(dry, by = c('art', 'treatment', 'phase', 'plot')) 



p <-
  ggplot(wet_dry, aes(Area19, Area18, colour = treatment, shape = phase)) +
  geom_point(size = 5) +
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA) +
  scale_colour_manual(values = c("0" = "steelblue2", "50" = "springgreen4", "90" = "orange2"), name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
  theme_classic() +
  #ylim(0, 0.45) +
  #xlim(0, 0.75) +
  theme(#panel.grid.major = element_blank(), 
    #panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.title=element_text(size=15), 
    legend.text=element_text(size=12),
    plot.title = element_text( size=40, hjust = 0.5),
    axis.title.x=element_text( size=12),
    axis.text.x=element_text(size = 12),
    axis.title.y = element_text( size=12),
    axis.line = element_line(linetype = "solid"),
    axis.text.y = element_text(size = 12)
  ) +
  ylab('Leaf area (cm2) 2018') +
  xlab ('Leaf area (cm2) 2019') +
  facet_wrap(~phase, scales = 'free')
  
tiff("Figures/single_LA_1819.tiff", units="in", width=24, height=6, res=300)
p
dev.off()
  


