library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)

# Need rutedata sheet 'frequency' for species abundances. Call it 'veg'
# Need rutedata sheet 'names' for taxa. Call it 'taxa'
# Need drought.plots for metadata on plots. Call it 'PlotID'

veg <- readRDS('cleandata/community.rds') #community data cleaned
PlotID <- read_xlsx('Data/drought.plots.xlsx')
taxa <- read_xlsx('Data/DE.1_Community.xlsx', sheet = 'names')

prep <- veg %>%
  filter (site == 'LYG') %>%                             
  replace (. == '.+' | . == 'plas', 1) %>%              # Replace <1 abundances with 1
  rename (Latin_name = species) %>%                    
  left_join (taxa, by='Latin_name') %>% 
  filter (!(Group=='bryophyte' | Group=='lichen')) %>% 
  rename (species = Latin_name) %>% 
  select (year, plot, species, cover) %>% 
  spread (species, cover, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  mutate_each (funs(as.numeric), 3:54) %>% 
  replace (is.na(.), 0) 

com <- metaMDS(prep %>%
                 select (-year, -plot))

env <- prep %>% 
  select (year, plot) %>% 
  mutate (year = as.character(year)) %>% 
  left_join (PlotID, by='plot') %>% 
  mutate(phase = fct_recode(phase, "Young" = 'pioneer', "Intermediate" = 'building', 'Old' = 'mature'))

data.scores <- as.data.frame(scores(com)) %>%   #Using the scores function from vegan to extract the site scores and convert to a data.frame
                 mutate(plot = prep$plot,
                        year = as.character(prep$year))%>% 
                 left_join(env, by=c('plot', 'year'))

data.scores$phase<-factor(data.scores$phase, levels=c("Young", "Intermediate", "Old"))
levels(data.scores$phase) <- c("Young", "Intermediate", "Old") 
  
species.scores <- as.data.frame(scores(com, "species")) %>% #Using the scores function from vegan to extract the species scores and convert to a data.frame
                  mutate(species = rownames(.))
  
reddata <- data.scores %>% 
  select(-phase) 


p <-
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2, shape= year, colour= treatment ),alpha=0.5 ) + 
  geom_point(data= reddata, colour = 'grey', size=2) +
  geom_point(size=3) +
  #geom_path(data=reddata,aes(x=NMDS1,y=NMDS2, group=plot)) +
  #scale_color_brewer(palette="Set1") +
  scale_colour_manual(values=c("0" = "steelblue2", "50" = "springgreen4", "90" = "orange2"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
  scale_shape_manual(values=c(16, 17, 15, 18))+
  coord_equal() +
  geom_path(data=data.scores,aes(x=NMDS1,y=NMDS2, group=plot)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.text.x = element_text(size = 14),
        #strip.background =element_blank()
        )+
  labs(shape="Year", colour="Treatment") +
  facet_wrap(~phase) 

tiff("Figures/ordination.tiff", units="in", width=12, height=5, res=300)
p
dev.off() 
