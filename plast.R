library(tidyverse)
library(readxl)
library(ggpubr)

# Data is first cleaned in PFT_Lygra_Datacleaning.R. Saved as cleandata/traits.rds

################ CALCULATING RDPIs ##############################

traits <- readRDS ('cleandata/traits.rds')

all_traits <- traits  %>% 
  filter(Year == '2018') %>% 
  rename(Plant_height = 'plantheight',
         Thickness = 'thickness')

all_traits$art <- paste(all_traits$Genus, all_traits$Species, sep='_')
all_traits$phase <- factor(all_traits$phase, levels = c("pioneer", "building", "mature"), labels = c("Pioneer", "Building", "Mature"))

phase_treatment <- all_traits %>% 
  group_by(phase, treatment, art, plot) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

phase_control <- all_traits %>% 
  group_by(phase, treatment, art, plot) %>% 
  filter (treatment == '0') %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)  %>% 
  rename (SLA_c = "SLA",
          LDMC_c = "LDMC",
          PH_c = "Plant_height",
          TH_c = "Thickness",
          control = "treatment",
          plot_c = "plot") 

plot <- all_traits %>% 
  group_by(treatment,plot, art, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

# Remove rows where a C plot compares with itselves
#To compare only plot means, remove art from group_by. To compare every species, keep art.
plast <- inner_join(plot, phase_control, by=c('phase', 'art')) %>% 
  filter(!(plot == "1.1." & plot_c == "1.1." | 
             plot == "2.2." & plot_c == "2.2." | 
             plot == "3.1." & plot_c == "3.1." | 
             plot == "4.3." & plot_c == "4.3." | 
             plot == "5.1." & plot_c == "5.1." | 
             plot == "6.2." & plot_c == "6.2." | 
             plot == "7.3." & plot_c == "7.3." | 
             plot == "8.3." & plot_c == "8.3." | 
             plot == "9.3." & plot_c == "9.3.")) %>% 
  group_by(treatment, plot, phase, art) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness', 'SLA_c', 'LDMC_c', 'PH_c', 'TH_c'), mean, na.rm=TRUE) %>% 
  mutate(rdpi_SLA = abs(abs(SLA_c - SLA)/SLA_c + SLA),
         rdpi_LDMC = abs(abs(LDMC_c - LDMC)/LDMC_c + LDMC),
         rdpi_PH = abs(abs(PH_c - Plant_height)/PH_c + Plant_height),
         rdpi_TH = abs(abs(TH_c - Thickness)/TH_c + Thickness)) 

#p <-
ggplot(plast, aes(treatment, log(rdpi_LDMC), fill = treatment)) +
  geom_boxplot() +
  scale_fill_brewer(direction = -1, name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
  theme(#panel.grid.major = element_blank(), 
    #panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text( size=20, hjust = 0.5),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y = element_text( size=14),
    axis.line = element_line(linetype = "solid")
  ) +
  #scale_x_discrete(labels = c("Pioneer","Building","Mature")) +
  xlab ('\nPost-fire phase') +
  labs(fill='Drought frequency') +
  ylab('Relative plasticity index (log)\n') +
  ggtitle('Plant height') +
  stat_compare_means( label = "p.signif", method='t.test', ref.group = '0') +
  facet_wrap(~phase)

tiff("PH_log.tiff", units="in", width=7, height=3, res=300)
p
dev.off() 







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

 #### Real RDPI

install.packages("remotes")
remotes::install_github("ameztegui/Plasticity")

devtools::install_github("ameztegui/Plasticity")

# Breakes on: ERROR: hard-coded installation path: please report to the package maintainer and use '--no-staged-install'
# * removing 'C:/Users/SiriVH/Documents/R/win-library/3.6/Plasticity'
# Error in i.p(...) : 
# (converted from warning) installation of package ‘C:/Users/SiriVH/AppData/Local/Temp/Rtmp8QjT5S/filefb44578741e/Plasticity_0.1.2.tar.gz’ had non-zero exit status

#Preøver en annen (lastet ned fra github)
source("RDPI.R")

#The function is called RDPI. It accepts 4 parameters:

#Data: the Dataset (all_traits)
#Trait: Column name of the dataset corresponding to measured trait (SLA)
#Env : column name of the dataset corresponding to the different levels of environmental conditions (treatment)
#Indiv : column name of the dataset corresponding to the individuals (or families or species depending of the experimental design) planted replicated over all environments (art)
#Thus dataset should contain 3 columns: one for the values of the measured trait, one for the environmental condition, one for the individuals (or families or species depending of the experimental design) hat is planted in each environment.

#The function return a two columns data frame:
  
#  First column is called "Indiv" and contained each replicated individual over all environments
#Second colum is called "RDPI" and contained the RDPI values

  RDPI(Data = Jack, Trait = Jack$LDMC, Env = Jack$steg, Indiv = Jack$art)
  RDPI(Jack, Trait = LDMC, Env = steg, Indiv = art)
  
  RDPI(Data = Jack, Trait: LDMC, Env: steg, Indiv: art)
  
  RDPI(Data = Mysla, LDMC, steg, art)
  
  RDPI(Data = all_traits, Trait = LDMC, Env = steg, Indiv = ID)

nei <- RDPI(Jack, "SLA",  "steg",  "art")

colnames(Mysla)
# trur eg må ha snittverdi per art per treatmentlevel.

plasticity <- all_traits %>% 
  group_by(art, treatment, phase) %>% 
  summarise_at(.vars = c('Plant_height', 'SLA', 'LDMC', 'Thickness'), mean, na.rm=TRUE)

write.csv(plasticity, file = "plasticity.csv")
write.csv(MyData, file = "MyData.csv")

Jack$steg <- paste (Jack$phase, Jack$treatment, sep = "_")

Mysla = Jack[, c('LDMC', 'steg', 'art')]
RDPI(Trait = Mysla$LDMC, Env = Mysla$steg, Indiv = Mysla$art)

RDPI( Mysla, LDMC, steg, art)

Jack2 <- Jack %>% 
  select(SLA,
         steg,
         art)

######

plast_p <- plast2 %>% 
  filter(phase == 'mature')

mod <- lm(SLA~treatment.x, data=plast_p)
summary(mod)
anova(mod)

m <- lme(SLA ~ treatment.x, random = ~1|phase/art, data = plast_p, na.action=na.omit)
summary(m)
anova(m)

compare_means(SLA~treatment.x, data=plast_p, method = "anova")
              

#We tested whether traits differ in their response to transplantation by using linear mixed effect models where PR was modeled as a function of trait, transplant type (warming vs. cooling), and their interaction with species and site as random effects to account for multiple samples from each species and site. We used the lmer function with Satterthwaite estimations for degrees of freedom for hypothesis testing from the lmerTest R package (Kuznetsova et al., 2017).

