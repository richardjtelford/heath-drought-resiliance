##¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤###
#### ORDINATION RESILIENCE ####
##¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤###


#### NMDS ####

library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)



# Need rutedata sheet 'frequency' for species abundances. Call it 'veg'
# Need rutedata sheet 'names' for taxa. Call it 'taxa'
# fOR LATER: Need drought.plots for metadata on plots. Call it 'PlotID' #environment variables

#community data cleaned
veg <- readRDS('cleandata/community.rds') 

#metadata about plots
PlotID <- read_xlsx('Data/drought.plots.xlsx') 

# just so we easily can filter out lichens and bryophytes:
#taxa <- read_xlsx('Data/DE.1_Community.xlsx', sheet = 'names') 

# this gives a matrix with year, plot, and site and then species as columns with cover in rows  
prep <- veg %>%
  filter (experiment == 'Resilience' | experiment == 'resilience') %>%          
  replace (. == '.+' | . == 'plas', 1) %>%  # Replace <1 abundances with 1
  filter ( cover > 5) %>% # this can be adjusted 
  #rename (Latin_name = species) %>%                    
  #left_join (taxa, by='Latin_name') %>% 
  #filter (!(Group=='bryophyte' | Group=='lichen')) %>% 
  #rename (species = Latin_name) %>% 
  select (year, plot, species, cover, site) %>% 
  group_by(year, plot, species, site) %>% 
  summarise_at(.vars = 'cover', sum, na.rm = TRUE) %>%
  spread (species, cover, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  #mutate_each (funs(as.numeric), 3:53) %>% 
  replace (is.na(.), 0) %>% 
  ungroup()


# extract only species-cover matrix
com <- metaMDS(prep %>%
                 select (-year, -plot, -site))

# extract metadata (year, plot, site). Add treatment  from plotID df
env <- prep %>% 
  select (year, plot, site) %>% 
  left_join (PlotID, by='plot')

# %>% 
#   #mutate (year = as.character(year)) %>% 
#   left_join (PlotID, by='plot') %>% 
#   mutate(phase = fct_recode(phase, "Young" = 'pioneer', "Intermediate" = 'building', 'Old' = 'mature'))

#Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores <- as.data.frame(scores(com)) %>%  
  mutate(plot = prep$plot,
         year = as.character(prep$year),
         site = prep$site) %>% 
  left_join(env, by=c('plot', 'year')) %>% 
  filter (!( site == 'TOR'))

#data.scores$phase<-factor(data.scores$phase, levels=c("Young", "Intermediate", "Old"))
#levels(data.scores$phase) <- c("Young", "Intermediate", "Old") 


#Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores <- as.data.frame(scores(com, "species")) %>% 
  mutate(species = rownames(.))

# make a data frame with the controls to make it possible to plot as a independent layer
reddata <- data.scores %>% 
   filter(treatment == 'C')


# NMDS plot in ggplot
#p <-
ggplot(data=data.scores %>% filter (treatment == 'B'), aes(x=NMDS1,y=NMDS2, shape= year, colour= site ), alpha=0.8 ) + 
  geom_point(data= reddata, size=3,  alpha = 0.3) + 
  #scale_colour_manual(values=c("BER" = "steelblue2", "YST" = "springgreen4", "NOV" = "orange2", "ROS" = "steelblue2", "SKO" = "springgreen4", "YST" = "orange2")) +
  geom_point(size=3) +
  #geom_path(data=reddata,aes(x=NMDS1,y=NMDS2, group=plot)) +
  #scale_color_brewer(palette="Set1") +
  #scale_colour_manual(values=c("0" = "steelblue2", "50" = "springgreen4", "90" = "orange2"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
  scale_shape_manual(values=c(16, 17, 15, 18))+
  coord_equal() +
  geom_path(data=data.scores %>% filter (treatment == 'B'),aes(x=NMDS1,y=NMDS2, group=plot), size = 1) +
  geom_path(data=reddata,aes(x=NMDS1,y=NMDS2, group=plot), size = 1, colour = 'grey', alpha = 0.1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.text.x = element_text(size = 14),
        #strip.background =element_blank()
  )+
  labs(shape="Year", colour="Site") #+
  facet_wrap (~site)

tiff("Figures/ordination_resilience_plot.tiff", units="in", width=6, height=6, res=300)
p
dev.off() 





#### PRC ####


# get information about burnt and control plots. Not necessary if the above is run
#PlotID <- read_xlsx('Data/drought.plots.xlsx') 

# just so we easily can filter out lichens and bryophytes:
taxa <- read_xlsx('Data/DE.1_Community.xlsx', sheet = 'names') 


# get community and make a species matrix
all <- readRDS('cleandata/community.rds')  %>%
  mutate(Latin_name = species) %>% 
  left_join(taxa, by = 'Latin_name')%>%  #community data cleaned 
  filter (experiment == 'Resilience' | experiment == 'resilience',
          !(plot %in% c('11.', '12.',  '13.',  '14.', '15.', '24.0.' ,'24.4.', '24.5.', '24.6.','24.9.', '24.10.', '24.11.', '24.12.')),
          !(Group == 'lichen' | Group == 'bryophyte')) %>% # remove extra plots that was not used
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(PlotID, by = c('site','plot')) %>% 
  ungroup() %>% 
  mutate(prc_treat = as.factor(ifelse(treatment == 'C', 'C', site)))

# extract time
year = as.factor(all$year)


#extract treatment
fire = as.factor(all$prc_treat) %>% 
  relevel(ref = 'C')


#extract community 
plants <- all %>% 
  select(8:106) %>% #ugly numbers
  replace (is.na(.), 0) 


# make rda model 
mod_rda <- rda(plants ~ fire*year + Condition(year), na.action = na.omit) 

mod_prc <- prc(response = plants,  treatment = fire, time = year, na.action = na.omit) 

# PRC plot
 
plot(mod_rda)
plot(mod_prc)


# get species scores and plot values
summary(mod_prc)

# # usefull stuff for later
# logabu <- colSums(plants)
# plot(mod, select = logabu > 200, legpos = NA)

# pcr function is really ugly and hard to modify. Want to make it in ggplot
# extract values which is what I will plot, and species scores which I will try to add on later

# species scores
test <- summary(mod_prc)
sp <- as.data.frame(test$sp)

# rank species scores
sp_all <- sp 


# plot values
 values <- as.data.frame(test$coefficients) %>% 
   gather(key = year, value = score) %>% 
   mutate(site = rep(c('BER', 'YST', 'NOV', 'ROS', 'SKO', 'TOR', 'YST'), times = 4))

 
 ggplot(values_all_p, aes(year, score, colour = plot)) +
   geom_point(size = 2) +
   geom_path(data = values_all_p, aes(x = year, y = score, group = plot), size = 1) +
   theme_classic() +
   scale_x_discrete (labels = c("Unburnt", "1", "2", "3"))  +
   geom_abline(intercept = 0, slope = 0) +
   ylab('PRC axis 1') +
   xlab('Years since fire') +
   facet_wrap(~site) 
 
### SEE BELOW PLOT
  # first, for site means, below there again for single plots

############# GGPLOT ##################


# plot
#need to go further down before running the plot

ggplot(values_all_p, aes(year, score, colour = plot)) +
  geom_point(size = 2) +
  geom_path(data = values_all_p, aes(x = year, y = score, group = plot), size = 1) +
  theme_classic() +
  scale_x_discrete (labels = c("Unburnt", "1", "2", "3"))  +
  geom_abline(intercept = 0, slope = 0) +
  ylab('PRC axis 1') +
  xlab('Years since fire') +
    facet_wrap(~site) 
  



# BUT, I want each site to compare to their own controls, not a avaraged control

### GOLTA ####


# get community and make a species matrix

all_GOL <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'GOL') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() 
  


# extract time

year_GOL = as.factor(all_GOL$year)


#extract treatment

fire_GOL = as.factor(all_GOL$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_GOL <- all_GOL %>% 
  select(31:73) %>% 
  replace (is.na(.), 0) 

# make model 

mod_GOL <- prc(response = plants_GOL,  treatment = fire_GOL, time = year_GOL, na.action = na.omit) 


# get species scores and plot values
test_GOL <- summary(mod_GOL)


# plot values
values_GOL <- as.data.frame(test_GOL$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'GOL')



# NOVELANDET ####

# get community and make a species matrix

all_NOV <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'NOV') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() 



# extract time

year_NOV = as.factor(all_NOV$year)


#extract treatment

fire_NOV = as.factor(all_NOV$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_NOV <- all_NOV %>% 
  select(31:57) %>% 
  replace (is.na(.), 0) 

# make model 

mod_NOV <- prc(response = plants_NOV,  treatment = fire_NOV, time = year_NOV, na.action = na.omit) 


# get species scores and plot values
test_NOV <- summary(mod_NOV)


# plot values
values_NOV <- as.data.frame(test_NOV$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'NOV')

# YSTEVIKA ####

# get community and make a species matrix

all_YST <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'YST') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() 



# extract time

year_YST = as.factor(all_YST$year)


#extract treatment

fire_YST = as.factor(all_YST$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_YST <- all_YST %>% 
  select(31:77) %>% 
  replace (is.na(.), 0) 

# make model 

mod_YST <- prc(response = plants_YST,  treatment = fire_YST, time = year_YST, na.action = na.omit) 


# get species scores and plot values
test_YST <- summary(mod_YST)


# plot values
values_YST <- as.data.frame(test_YST$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'YST')

# ROSSVOLLOYA ####


# get community and make a species matrix

all_ROS <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'ROS') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() 



# extract time

year_ROS = as.factor(all_ROS$year)


#extract treatment

fire_ROS = as.factor(all_ROS$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_ROS <- all_ROS %>% 
  select(31:64) %>% 
  replace (is.na(.), 0) 

# make model 

mod_ROS <- prc(response = plants_ROS,  treatment = fire_ROS, time = year_ROS, na.action = na.omit) 


# get species scores and plot values
test_ROS <- summary(mod_ROS)


# plot values
values_ROS <- as.data.frame(test_ROS$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'ROS')

# BERGSNOVA ####


# get community and make a species matrix

all_BER <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'BER') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() 



# extract time

year_BER = as.factor(all_BER$year)


#extract treatment

fire_BER = as.factor(all_BER$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_BER <- all_BER %>% 
  select(31:50) %>% 
  replace (is.na(.), 0) 

# make model 

mod_BER <- prc(response = plants_BER,  treatment = fire_BER, time = year_BER, na.action = na.omit) 


# get species scores and plot values
test_BER <- summary(mod_BER)


# plot values
values_BER <- as.data.frame(test_BER$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'BER')

# TORSOYA ####


# get community and make a species matrix

all_TOR <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'TOR') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() 



# extract time

year_TOR = as.factor(all_TOR$year)


#extract treatment

fire_TOR = as.factor(all_TOR$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_TOR <- all_TOR %>% 
  select(31:62) %>% 
  replace (is.na(.), 0) 

# make model 

mod_TOR <- prc(response = plants_TOR,  treatment = fire_TOR, time = year_TOR, na.action = na.omit) 


# get species scores and plot values
test_TOR <- summary(mod_TOR)


# plot values
values_TOR <- as.data.frame(test_TOR$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'TOR')

# SKOTSVAR ####


# get community and make a species matrix

all_SKO <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'SKO' & (experiment == 'Resilience' | experiment == 'resilience')) %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() 



# extract time

year_SKO = as.factor(all_SKO$year)


#extract treatment

fire_SKO = as.factor(all_SKO$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_SKO <- all_SKO %>% 
  select(31:65) %>% 
  replace (is.na(.), 0) 

# make model 

mod_SKO <- prc(response = plants_SKO,  treatment = fire_SKO, time = year_SKO, na.action = na.omit) 


# get species scores and plot values
test_SKO <- summary(mod_SKO)


# plot values
values_SKO <- as.data.frame(test_SKO$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'SKO')


 # ALL SITES ####


values_all <- rbind (values_GOL, values_NOV, values_YST, values_ROS, values_BER, values_TOR, values_SKO) 

# bind_rows instead of rbind, then you can drop mutate for site names, and use bind_roms(SKO = values_SKO (...), .id = 'site')
## you van make an object a prs object by: object <- "prc"

values_all$site <- factor(values_all$site, levels =  c('GOL', 'NOV', 'YST', 'ROS', 'BER', 'TOR', 'SKO'))

# AND NOW, GO BACK UP TO GGPLOT

##################### FOR SINGLE PLOTS: ####

# get information about burnt and control plots

# GOLTA ####


all_GOL_p <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'GOL') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() %>% 
  mutate(treatment = as.factor(ifelse(treatment == 'C', 'C', plot)))



# extract time

year_GOL_p = as.factor(all_GOL_p$year)


#extract treatment

fire_GOL_p = as.factor(all_GOL_p$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_GOL_p <- all_GOL_p %>% 
  select(31:73) %>% 
  replace (is.na(.), 0) 

# make model 

mod_GOL_p <- prc(response = plants_GOL_p,  treatment = fire_GOL_p, time = year_GOL_p, na.action = na.omit) 


# get species scores and plot values
test_GOL_p <- summary(mod_GOL_p)

levels(fire_GOL_p)

# plot values
values_GOL_p <- as.data.frame(test_GOL_p$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'GOL',
         plot = rep(c('19.0.', '19.2.', '19.4.', '19.8.', '19.9.'), times = 4)
         )
        
# NOVELANDET ####

# get community and make a species matrix

all_NOV_p <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'NOV') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() %>% 
  mutate(treatment = as.factor(ifelse(treatment == 'C', 'C', plot)))



# extract time

year_NOV_p = as.factor(all_NOV_p$year)


#extract treatment

fire_NOV_p = as.factor(all_NOV_p$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_NOV_p <- all_NOV_p %>% 
  select(31:57) %>% 
  replace (is.na(.), 0) 

# make model 

mod_NOV_p <- prc(response = plants_NOV_p,  treatment = fire_NOV_p, time = year_NOV_p, na.action = na.omit) 


# get species scores and plot values
test_NOV_p <- summary(mod_NOV_p)

levels(fire_NOV_p)

# plot values
values_NOV_p <- as.data.frame(test_NOV_p$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'NOV',
         plot = rep(c('20.1.', '20.2.', '20.3.', '20.4.', '20.5.'), times = 4)
         )

# YSTEVIKA ####

# get community and make a species matrix

all_YST_p <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'YST') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() %>% 
  mutate(treatment = as.factor(ifelse(treatment == 'C', 'C', plot)))



# extract time

year_YST_p = as.factor(all_YST_p$year)


#extract treatment

fire_YST_p = as.factor(all_YST_p$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_YST_p <- all_YST_p %>% 
  select(31:77) %>% 
  replace (is.na(.), 0) 

# make model 

mod_YST_p <- prc(response = plants_YST_p,  treatment = fire_YST_p, time = year_YST_p, na.action = na.omit) 


# get species scores and plot values
test_YST_p <- summary(mod_YST_p)

levels(fire_YST_p)

# plot values
values_YST_p <- as.data.frame(test_YST_p$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'YST',
         plot = rep(c('21.0.', '21.1.', '21.7.', '21.8.', '21.9.'), times = 4)
         )

# ROSSVOLLOYA ####


# get community and make a species matrix

all_ROS_p <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'ROS') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() %>% 
  mutate(treatment = as.factor(ifelse(treatment == 'C', 'C', plot)))



# extract time

year_ROS_p = as.factor(all_ROS_p$year)


#extract treatment

fire_ROS_p = as.factor(all_ROS_p$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_ROS_p <- all_ROS_p %>% 
  select(31:64) %>% 
  replace (is.na(.), 0) 

# make model 

mod_ROS_p <- prc(response = plants_ROS_p,  treatment = fire_ROS_p, time = year_ROS_p, na.action = na.omit) 


# get species scores and plot values
test_ROS_p <- summary(mod_ROS_p)

levels(fire_ROS_p)

# plot values
values_ROS_p <- as.data.frame(test_ROS_p$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'ROS',
         plot = rep(c('22.0.', '22.1.', '22.2', '22.4.', '22.7.'), times = 4)
         )

# BERGSNOVA ####

# get community and make a species matrix

all_BER_p <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'BER') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() %>% 
  mutate(treatment = as.factor(ifelse(treatment == 'C', 'C', plot)))



# extract time

year_BER_p = as.factor(all_BER_p$year)


#extract treatment

fire_BER_p = as.factor(all_BER_p$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_BER_p <- all_BER_p %>% 
  select(31:50) %>% 
  replace (is.na(.), 0) 

# make model 

mod_BER_p <- prc(response = plants_BER_p,  treatment = fire_BER_p, time = year_BER_p, na.action = na.omit) 


# get species scores and plot values
test_BER_p <- summary(mod_BER_p)

levels(fire_BER_p)

# plot values
values_BER_p <- as.data.frame(test_BER_p$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'BER',
         plot= rep(c('23.0.', '23.6.', '23.7.', '23.8.', '23.9.'), times = 4)
         )

# TORSOYA ####


# get community and make a species matrix

all_TOR_p <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'TOR') %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() %>% 
  mutate(treatment = as.factor(ifelse(treatment == 'C', 'C', plot)))



# extract time

year_TOR_p = as.factor(all_TOR_p$year)


#extract treatment

fire_TOR_p = as.factor(all_TOR_p$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_TOR_p <- all_TOR_p %>% 
  select(31:62) %>% 
  replace (is.na(.), 0) 

# make model 

mod_TOR_p <- prc(response = plants_TOR_p,  treatment = fire_TOR_p, time = year_TOR_p, na.action = na.omit) 


# get species scores and plot values
test_TOR_p <- summary(mod_TOR_p)

levels(fire_TOR_p)

# plot values
values_TOR_p <- as.data.frame(test_TOR_p$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'TOR',
         plot = rep(c('24.13.', '24.14.', '24.15.', '24.16.', '24.17.'), times = 4))

# SKOTSVAR ####


# get community and make a species matrix

all_SKO_p <- readRDS('cleandata/community.rds') %>%  #community data cleaned 
  filter (site == 'SKO' & (experiment == 'Resilience' | experiment == 'resilience')) %>% 
  replace (. == '.+' | . == 'plas', 1) %>% # Replace <1 abundances with 1
  #filter ( cover > 5) %>%  
  #filter(!(species == 'Cladonia_sp')) %>% 
  #select (species, cover) %>% 
  spread(species, cover) %>% 
  left_join(treatment, by = c('site','plot')) %>% 
  ungroup() %>% 
  mutate(treatment = as.factor(ifelse(treatment == 'C', 'C', plot)))



# extract time

year_SKO_p = as.factor(all_SKO_p$year)


#extract treatment

fire_SKO_p = as.factor(all_SKO_p$treatment) %>% 
  relevel(ref = 'C')


#extract communuity

plants_SKO_p <- all_SKO_p %>% 
  select(31:65) %>% 
  replace (is.na(.), 0) 

# make model 

mod_SKO_p <- prc(response = plants_SKO_p,  treatment = fire_SKO_p, time = year_SKO_p, na.action = na.omit) 


# get species scores and plot values
test_SKO_p <- summary(mod_SKO_p)

levels(fire_SKO_p)

# plot values
values_SKO_p <- as.data.frame(test_SKO_p$coefficients) %>% 
  gather(key = year, value = score) %>% 
  mutate(site = 'SKO',
         plot = rep(c('25.1.', '25.2.', '25.3.', '25.4.', '25.5.'), times = 4)
         )

values_all_p <- rbind (values_GOL_p, values_NOV_p, values_YST_p, values_ROS_p, values_BER_p, values_TOR_p, values_SKO_p) 

values_all_p$site <- factor(values_all_p$site, levels =  c('GOL', 'NOV', 'YST', 'ROS', 'BER', 'TOR', 'SKO'))




#### LGV PhD #####

LGV <- read_xlsx('Data/LGV_Kontroll_Nerlands?y_2007.xlsx') %>% 
  replace (is.na(.), 0)

LGV_site <- LGV %>% 
  select(Site)

LGV_plants <- LGV %>% 
  select(!(Site)) %>% 
           select(!(Block)) %>% 
                    select(!(Plot)) 
        
LGV_plot <- LGV %>% 
  select(Plot) %>% 
  mutate(Plot = as.factor(Plot))

LGV_block <- LGV %>% 
  select(Block) %>% 
  mutate(Block = as.character(Block))


LGV_rda <- rda(LGV [,-1:3] ~ LGV_block*LGV_plot)

plot(LGV_rda)
    


