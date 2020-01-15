            
                                  # Last worked on November 22nd 2019

library(readxl)
library(tidyverse)


#### Import data ####

# period used as comma mark in raw data, it confuses R. Using colnames to stop R from guessing what the columns are, and then forcing it to get it right

traits <- read_xlsx('Data/DE.5_Plant_functional_traits.xlsx', sheet = 'Ark1', na = c('NA', 'N/A'), col_types = 'text') %>%  #main data  
  mutate_at(
    vars(Plant_height:`Thickness_2...18`), as.numeric    # force the right column types
  ) %>% 
  rename(Wetmass = Wet_mass,
         plantheight = Plant_height) %>% 
  mutate(thickness = (Thickness_1 + Thickness_2...17 + Thickness_2...18)/3,
         plot = str_replace(plot, pattern = "L", replacement = ""),   #Remove L in start of plotnr 2018
         plot = str_replace(plot, pattern = "$" , replacement =  "."), #Add '.' in end of plotnr 2018
         plot = recode(plot,   
                       '2.3..' = '2.3.',
                       '3.1..' = '3.1.',
                       '2.2..' = '2.2.',
                       '.1.1.' = '1.1.',
                       '1.2..' = '1.2.',
                       '1.3..' = '1.3.',
                       '2.1..' = '2.1.',
                       '1.1..' = '1.1.',
                       '3.3..' = '3.3.')) 
        

#col_types = c(rep('text', 12), rep('numeric', 7), 'text'


# TASK 1 was changing classes. This is now in the import code.

#### 100 degrees ####
#some leaves were dried at 100 degrees instead of 55 degrees. plot weight to check.

#hundred <- read_xlsx('Data/DE.5_Plant_functional_traits.xlsx', sheet = 'ID_dried_105_C') #list of IDs that's been dried to warm

#hundred$dried <- 1 #Indicates to high temperature

#traits <- left_join(traits, hundred, by='ID') 

#traits <- traits %>%
#  mutate(dried = if_else(is.na(dried), 0, dried)) #0 = right temperature, 1 = wrong #temperature

#traits$dried <- as.character(traits$dried)

#ggplot(traits, aes(dried, Drymass)) +  #Does not look great, but 0 has some outliers
#  geom_boxplot()

#right.temp <- traits %>% 
#  filter(dried == '0')
#wrong.temp <- traits %>%
#  filter(dried == 1)

#t.test(right.temp$Drymass, wrong.temp$Drymass)   #significant, but this has all kind of species from all kind of treatment in tight, and only a small subset in wrong

#to <- inner_join(wrong.temp,right.temp, by=c('plot', 'Species')) #new dataframe with only comparable samples
       
#t.test(to$Drymass.x, to$Drymass.y)                  #p-value = 0.9887
#t.test(to$Drymass.x, to$Drymass.y, paired = TRUE)    #p-value = 0.6593


   
      ## CONCLUSION: can keep samples from wrong temperature

#Ctrl+shift+C: hashtag a big chunck

#### MASS ####

#Check outliers

    # DRYMASS - the code right now fix the data set per 22.11.2019

hist(traits$Drymass)
max(traits$Drymass, na.rm=TRUE) #Vekt p? nesten 1 gram.
which(traits$Drymass == 0.98255) #bruk row number fr? her til ? finne wetmass i neste
traits[336,'Wetmass'] #wetmass er lavere enn drymass

    ## CONCLUSION: drymass is wrong with one digit.Checked the others, those are fine.

traits[336, 'Drymass'] = traits[336, 'Drymass']/10 #corrected

#1: traits[336, 'Drymass'] = 0.98255  #Adjusted with one digit
#2: traits[499,'Drymass']= 0.38633 #OK

  
    #WETMASS

hist(traits$Wetmass) 
max(traits$Wetmass, na.rm=TRUE)
which(traits$Wetmass == 0.9713) 
traits[499,'Drymass'] 
#looks good

    #Plant height

hist(traits$plantheight)
max(traits$plantheight, na.rm=TRUE)
which(traits$plantheight == 81) 
traits[2170,'Species'] 
#looks good

    #Thickness 

hist(traits$thickness) 
max(traits$thickness, na.rm = TRUE) #tykke Juncuser og Callunaer


#### Expanding data set ####

ggplot(traits, aes(Wetmass, Drymass)) +
  geom_point(size=3) +
  geom_abline(intercept = 0, slope = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

traits <- traits %>% 
  mutate(LDMC = ifelse(Drymass > Wetmass, (Drymass*100)/Wetmass, (Drymass*1000)/Wetmass)) #Some drymass>wetmass, all have 6 decimal places for wet mass and only five decimal places for drymass. Conclusion: missed a zero in dry mass (0,003.. instead og 0,03...). *1000 because the unit is mg/g



# Leaf area
# 2018 data are calculated by Aud Halbritter and 2019 data by Sonya Greange

load('Data/LeafAreaSiri_2018.Rdata')
load('Data/LeafArea2019.Rdata')

leafarea <- rbind(LeafArea2019, LeafAreaSiri_2018) %>% 
  mutate(ID = str_replace(ID, pattern = ".j", replacement = ""))

hist(leafarea$Area_cm2) #up to 30 cm2

big <- leafarea %>%    #24/1466 scans er st?rre enn 10 cm2. 
  filter (Area_cm2 > 10)
# Alle 24 er ok. Alle er graminider, stort sett rome eller molinia. 


traits <- left_join(traits, leafarea, by='ID')

# SLA

traits$SLA <- traits$Area_cm2/traits$Drymass

saveRDS(traits, 'cleandata/traits.rds')

######## working line 23.11.2019 #####

### Calculating CWM ####

arter <- read_xlsx('Artsliste2018.xlsx', sheet = 1)
grupper <- read_xlsx('Artsliste2018.xlsx', sheet = 'fun_group')


# andel art per plot*traitet

arter$cover <- as.numeric(arter$cover)
arter <- left_join(grupper, arter, by=c('Genus', 'Species')) #for å kunne filtrere på fun.grupper

# dekningsgraden i plot, altså total cover av vakulære grupper
plot <- arter %>% 
  filter(!(Group=='bryophytes')) %>% 
  group_by(plot) %>% 
  summarise(cover = sum(cover, na.rm = TRUE))

# snittverdien til kvar art i kvart plot
art_mean <- traits %>% 
  group_by(.dots = c('plot', 'Genus', 'Species')) %>% 
  summarise_at(.vars = c('SLA', 'Plant_height', 'Thickness', 'LDMC'), mean)

art <- left_join(art_mean, arter, by = c('plot', 'Genus', 'Species'))
art <- left_join(art, plot, by = c('plot'))
art$prop <- art$cover.x / art$cover.y

art$prop_SLA <- art$prop * art$SLA
art$prop_LDMC <- art$prop * art$LDMC
art$prop_PH <- art$prop * art$Plant_height
art$prop_T <- art$prop * art$Thickness

CWM <- art %>% 
  group_by(plot) %>% 
  summarise_at(.vars = c('prop_SLA', 'prop_PH', 'prop_T', 'prop_LDMC'), sum, na.rm=T)

ID <- read_xlsx('drought.plots.xlsx')
ID$phase<-factor(ID$phase, levels=c("pioneer", "building", "mature"))

CWM <- left_join(CWM, ID, by='plot')

xlsx::write.xlsx(as.data.frame(CWM), file ="CWM_traits.xlsx", sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)



#### working line ######

## for graminoids

plot_g <- arter %>% 
  filter(Group=='graminoid') %>% 
  group_by(plot) %>% 
  summarise(cover = sum(cover, na.rm = TRUE))

art_g <- left_join(art_mean, arter, by = c('plot', 'Genus', 'Species'))
art_g <- left_join(art_g, plot_g, by = c('plot'))
art$prop <- art$cover.x / art$cover.y

art$prop_SLA <- art$prop * art$SLA
art$prop_LDMC <- art$prop * art$LDMC
art$prop_PH <- art$prop * art$Plant_height
art$prop_T <- art$prop * art$Thickness

CWM <- art %>% 
  group_by(plot) %>% 
  summarise_at(.vars = c('prop_SLA', 'prop_PH', 'prop_T', 'prop_LDMC'), sum, na.rm=T)

ID <- read_xlsx('drought.plots.xlsx')
ID$phase<-factor(ID$phase, levels=c("pioneer", "building", "mature"))

CWM <- left_join(CWM, ID, by='plot')

xlsx::write.xlsx(as.data.frame(CWM), file ="CWM_traits.xlsx", sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE) 



#Legg p? treatment


fukt <- read_xlsx('sesong.xlsx', sheet = 1)

traits <- left_join(traits, fukt, by = 'plot')
CWM <- left_join(CWM, fukt, by = 'plot')

traits <- traits %>%
  filter(sensor == 'soil.moist.15')

CWM <- CWM %>%
  filter(sensor == 'soil.moist.15')




 ###### Export data ######

xlsx::write.xlsx(as.data.frame(CWM), file ="CWM_Lygra_aug18.xlsx", sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE) 

xlsx::write.xlsx(as.data.frame(traits), file ="traits_Lygra_aug18.xlsx", sheetName = "Sheet1",  col.names = TRUE, row.names = TRUE, append = FALSE) 
