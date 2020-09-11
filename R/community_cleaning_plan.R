

# clean community data


clean_community_plan = drake_plan(
# Cleaning species names (when species list are clean, check it with a proper taxonomy list)
  
comm = comm0 %>% 
  mutate(species = case_when(
    species == "cladonia_sp" ~ "Cladonia_sp",
    species == "llex_aquifolium" ~ "Ilex_aquifolium",
    TRUE ~ species
  )) %>% 
  left_join(spp_names, by= 'species') %>% 
  mutate(species = coalesce(correct_name, species)) %>%  #jumps to second if first in NA, keep first if it is not NA
  select(-correct_name) %>% 
  mutate(plot = if_else(site == 'BUO' & year == '2016', #BUO had wrong plot numbers in 2016. Fix that
                        true = recode(plot,   
                                      '10.1.' = '11.1.',
                                      '10.2.' = '10.1.',
                                      '10.3.' = '12.1.',
                                      '11.1.' = '10.3.',
                                      '11.2.' = '10.2.',
                                      '11.3.' = '11.2.',
                                      '12.1.' = '12.2.',
                                      '12.2.' = '11.3.',
                                      '12.3.' = '12.3.'
                        ), false = plot)) %>% 
  mutate(plot = if_else(site == 'ROS' & year == '2018', #ROS had wrong plot numbers in 2018. Fix that
                       true = recode(plot,   
                                     '22.1.' = '22.3.',
                                     '22.3.' = '22.1.',
                                     '22.4.' = '22.6.',
                                     '22.5.' = '22.4.',
                                     '22.6.' = '22.7.',
                                     '22.7.' = '22.5.',
                                     '22.9.' = '22.10.',
                                     '22.10.' = '22.9.'
                       ), false = plot),
         plot = ifelse(site == 'ROS' & plot == '22.10', '22.0.', plot),
         plot = ifelse(site == 'SKO' & plot == '25.10', '25.0.', plot),
         plot = ifelse(site == 'TOR' & year == '2018' & plot == '14', '24.14.', plot),
         plot = ifelse(site == 'TOR' & year == '2018' & plot == '16', '24.16.', plot),
         plot = ifelse(site == 'ROS' & year == '2017' & plot == '22.9.', '22.0.', plot),
         plot = ifelse(site == 'ROS' & year == '2017' & plot == '22.0', '22.9.', plot),
         plot = str_replace(plot, pattern = "$" , replacement =  "."),       # This fix the plot number issue on TOR 
         plot = str_replace(plot, pattern = "\\.\\.", replacement = ".")) %>% 
  mutate(plot = if_else(site == 'SKO' & !year == '2017' , #SKO were misplaced with one in three years
                        true = recode(plot,   
                                      '25.2.' = '25.1.',
                                      '25.3.' = '25.2.',
                                      '25.4.' = '25.3.',
                                      '25.5.' = '25.4.',
                                      '25.6.' = '25.5.',
                                      '25.7.' = '25.6.',
                                      '25.8.' = '25.7.',
                                      '25.9.' = '25.8.',
                                      '25.10.' = '25.9.',
                                      '25.0.' = '25.9.',
                                      '25.1.' = '25.0.'  
                                      ),  false = plot)) %>%  
  # fixing species
  mutate(species = case_when(
    site == 'BUO' & species %in% c('Agrostis_canina', 'Agrostis_capillaris', 'Agrostis_sp') ~ 'Agrostis_capillaris', 
    site == 'NOV' & species == 'Agrostis_canina' ~ 'Agrostis_vinealis',
    site == 'GOL' & species == 'Lycopodium_annitonum' ~ 'Lycopodum_clavatum', 
    site == 'GOL' & species %in% c('Polygala_vulgaris', 'Polygala_sp') ~ 'Polygala_serpyllifolia',
    site == 'GOL' & species == 'Diphasiastrum_sp' ~ 'Diphasiastrum_alpinum', 
    (site == 'NOV' & species == 'Viola_sp') | (site == 'BUO' & species %in% c('Viola_canina1', 'Viola_sp', 'Viola_riviniana')) ~ 'Viola_canina',
    site == 'TOR' & species == 'Carex_binervis' ~ 'Carex_vaginata',
    site == 'BUO' & species %in% c('Betula_nana', 'Betula_sp') ~ 'Betula_pubescens',
    site == 'BUO' & species == 'Vicia_sp' ~ 'Vicia_cracca', 
    site == 'BUO' & species %in% c('Polygala_serpyllifolia', 'Polygala_sp') ~ 'Polygala_vulgaris', 
    site == 'BUO' & species %in% c('Galium_saxatile', 'Galium_sp') ~ 'Galium_boreale', 
    site %in% c('YST', 'LYG') & species == 'Polygala_vulgaris' ~ 'Polygala_serpyllifolia', 
    site == 'LYG' & plot == '7.1.' & species == 'Carex_nigra' ~ 'Carex_echinata', 
    site == 'LYG' & plot == '1.1.' & species == 'Festuca_sp' & year == '2017' ~ 'Festuca_vivipara',
    site == 'LYG' & plot %in% c('1.2.', '3.1.') & species == 'Festuca_rubra' & year == '2019' ~ 'Festuca_vivipara',
    site == 'LYG'  & species == 'Luzula_sylvatica' & year == '2019' ~ 'Luzula_pilosa',
    site == 'LYG' & plot == '2.2.' & species == 'Hylocomium_splendens' & year == '2018' ~ 'Hypnum_sp',
    site == 'LYG' & plot == '2.3.' & species == 'Luzula_pilosa' & year == '2018' ~ 'Luzula_multiflora',
    site == 'LYG' & plot == '3.2.' & species == 'Carex_sp' & year == '2019' ~ 'Carex_pilulifera',
    site == 'LYG' & plot %in% c('4.3.', '6.2.', '6.3.') & species == 'Carex_sp' & year == '2016' ~ 'Carex_pilulifera',
    site == 'LYG' & plot == '3.2.' & species == 'Lotus_corniculatus' & year == '2018' ~ 'Galium_saxatile',
    site == 'LYG' & plot == '3.3.' & species == 'Luzula_multiflora_cf' & year == '2019' ~ 'Luzula_multiflora',
    site == 'LYG' & plot == '5.1.' & species == 'Carex_binervis' & year == '2018' ~ 'Eriophorum_angustifolium',  
    site == 'LYG' & plot == '5.2.' & species == 'Carex_sp' & year == '2017' ~ 'Eriophorum_angustifolium',
    site == 'LYG' & plot == '7.3.' & species == 'Eriophorum_vaginatum' & year %in% c('2017', '2016') ~ 'Eriophorum_angustifolium',  
    site == 'LYG' & plot == '9.2.' & species == 'Eriophorum_vaginatum' & year == '2019' ~ 'Eriophorum_angustifolium',
    site == 'LYG' & plot == '5.2.' & species == 'Carex_flava' & year == '2016' ~ 'Carex_pilulifera',
    site == 'LYG' & plot %in% c('5.2.', '5.3.') & species == 'Carex_nigra' & year == '2016' ~ 'Carex_panicea',
    site == 'LYG' & plot %in% c('5.3.', '6.1.') & species == 'Carex_echinata' & year == '2016' ~ 'Carex_pilulifera',
    site == 'LYG' & plot %in% c('5.2.', '5.3.') & species == 'Juncus_sp' & year == '2016' ~ 'Juncus_squarrosus',
    site == 'LYG' & plot == '7.1.' & species %in% c('Carex_binervis', 'Carex_pilulifera') & year == '2017' ~ 'Carex_echinata',
    site == 'GOL' & plot %in% c('19.4.', '19.7.', '19.8.') &  species == 'Carex_sp' & year == '2016' ~ 'Carex_pilulifera', 
    site == 'GOL' & plot == '19.8.' &  species == 'Festuca_sp' & year == '2017' ~ 'Festuca_rubra', 
    site == 'NOV' & plot == '20.2.' &  species == '??' & year == '2018' ~ 'Unknown_herb',
    site == 'NOV' & plot == '20.4.' &  species == 'Rhytidiadelphus_squarrosus' & year == '2017' ~ 'Rhytidiadelphus_loreus',
    site == 'NOV' & plot == '20.9.' &  species == 'Anthoxanthum_odoratum' & year == '2017' ~ 'Agrostis_capillaris',
    site == 'NOV' & plot == '20.9.' &  species == 'Carex_sp' & year %in% c('2016', '2018') ~ 'Carex_pilulifera',
    site == 'NOV' & plot == '20.9.' &  species == 'Rhytidiadelphus_loreus' & year == '2018' ~ 'Rhytidiadelphus_squarrosus',
    TRUE ~ species )) %>% 
  mutate(experiment = tolower(experiment)) %>% #fix case error
  filter(experiment == "resilience") %>% 
  group_by(site, plot, year, species, group) %>% 
  summarise(cover = sum(cover, na.rm = TRUE)) 

# comm_old = comm0 %>% 
#   left_join(spp_names, by= 'species') %>% 
#   mutate(species = coalesce(correct_name, species)) %>%  #jumps to second if first in NA, keep first if it is not NA
#   select(-correct_name) %>% 
#   mutate(plot = if_else(site == 'BUO' & year == '2016', #BUO had wrong plot numbers in 2016. Fix that
#                         true = recode(plot,   
#                                       '10.1.' = '11.1.',
#                                       '10.2.' = '10.1.',
#                                       '10.3.' = '12.1.',
#                                       '11.1.' = '10.3.',
#                                       '11.2.' = '10.2.',
#                                       '11.3.' = '11.2.',
#                                       '12.1.' = '12.2.',
#                                       '12.2.' = '11.3.',
#                                       '12.3.' = '12.3.'
#                         ), false = plot)) %>% 
#   mutate(plot = if_else(site == 'ROS' & year == '2018', #ROS had wrong plot numbers in 2018. Fix that
#                         true = recode(plot,   
#                                       '22.1.' = '22.3.',
#                                       '22.3.' = '22.1.',
#                                       '22.4.' = '22.6.',
#                                       '22.5.' = '22.4.',
#                                       '22.6.' = '22.7.',
#                                       '22.7.' = '22.5.',
#                                       '22.9.' = '22.10.',
#                                       '22.10.' = '22.9.'
#                         ), false = plot),
#          plot = ifelse(site == 'ROS' & plot == '22.10', '22.0.', plot),
#          plot = ifelse(site == 'SKO' & plot == '25.10', '25.0.', plot),
#          plot = ifelse(site == 'TOR' & year == '2018' & plot == '14', '24.14.', plot),
#          plot = ifelse(site == 'TOR' & year == '2018' & plot == '16', '24.16.', plot),
#          plot = ifelse(site == 'ROS' & year == '2017' & plot == '22.9.', '22.0.', plot),
#          plot = ifelse(site == 'ROS' & year == '2017' & plot == '22.0', '22.9.', plot),
#          plot = str_replace(plot, pattern = "$" , replacement =  "."),       # This fix the plot number issue on TOR 
#          plot = str_replace(plot, pattern = "\\.\\.", replacement = ".")) %>% 
#   mutate(plot = if_else(site == 'SKO' & !year == '2017' , #SKO were misplaced with one in three years
#                         true = recode(plot,   
#                                       '25.2.' = '25.1.',
#                                       '25.3.' = '25.2.',
#                                       '25.4.' = '25.3.',
#                                       '25.5.' = '25.4.',
#                                       '25.6.' = '25.5.',
#                                       '25.7.' = '25.6.',
#                                       '25.8.' = '25.7.',
#                                       '25.9.' = '25.8.',
#                                       '25.10.' = '25.9.',
#                                       '25.0.' = '25.9.',
#                                       '25.1.' = '25.0.'  
#                         ),  false = plot)) %>%  # fixing species (should change to case when)
#   mutate(new.species = coalesce(ifelse(site == 'BUO' & (species == 'Agrostis_canina' | species == 'Agrostis_capillaris' | species == 'Agrostis_sp'), 'Agrostis_capillaris', 
#                                        ifelse(site == 'NOV' & species == 'Agrostis_canina', 'Agrostis_vinealis', 
#                                               ifelse(site == 'GOL' & species == 'Lycopodium_annitonum', 'Lycopodum_clavatum', 
#                                                      ifelse(site == 'GOL' & (species == 'Polygala_vulgaris' | species == 'Polygala_sp'), 'Polygala_serpyllifolia', 
#                                                             ifelse(site == 'GOL' & species == 'Diphasiastrum_sp', 'Diphasiastrum_alpinum', 
#                                                                    ifelse((site == 'NOV' & species == 'Viola_sp') | (site == 'BUO' & (species == 'Viola_canina1' | species == 'Viola_sp' | species == 'Viola_riviniana')), 'Viola_canina',
#                                                                           ifelse(site == 'TOR' & species == 'Carex_binervis', 'Carex_vaginata',
#                                                                                  ifelse(site == 'BUO' & (species == 'Betula_nana' | species == 'Betula_sp'), 'Betula_pubescens',
#                                                                                         ifelse(site == 'BUO' & species == 'Vicia_sp', 'Vicia_cracca', 
#                                                                                                ifelse(site == 'BUO' & (species == 'Polygala_serpyllifolia' | species == 'Polygala_sp'), 'Polygala_vulgaris', 
#                                                                                                       ifelse(site == 'BUO' & (species == 'Galium_saxatile' | species == 'Galium_sp'), 'Galium_boreale', 
#                                                                                                              ifelse((site == 'YST' | site == 'LYG') & species == 'Polygala_vulgaris', 'Polygala_serpyllifolia', 
#                                                                                                                     ifelse(site == 'LYG' & plot == '7.1.' & species == 'Carex_nigra', 'Carex_echinata', 
#                                                                                                                            ifelse(site == 'LYG' & plot == '1.1.' & species == 'Festuca_sp' & year == '2017', 'Festuca_vivipara',
#                                                                                                                                   ifelse(site == 'LYG' & (plot == '1.2.' | plot == '3.1.') & species == 'Festuca_rubra' & year == '2019', 'Festuca_vivipara',
#                                                                                                                                          ifelse(site == 'LYG'  & species == 'Luzula_sylvatica' & year == '2019', 'Luzula_pilosa',
#                                                                                                                                                 ifelse(site == 'LYG' & plot == '2.2.' & species == 'Hylocomium_splendens' & year == '2018', 'Hypnum_sp',
#                                                                                                                                                        ifelse(site == 'LYG' & plot == '2.3.' & species == 'Luzula_pilosa' & year == '2018', 'Luzula_multiflora',
#                                                                                                                                                               ifelse(site == 'LYG' & plot == '3.2.' & species == 'Carex_sp' & year == '2019', 'Carex_pilulifera',
#                                                                                                                                                                      ifelse(site == 'LYG' & (plot == '4.3.' | plot == '6.2.' | plot == '6.3.') & species == 'Carex_sp' & year == '2016', 'Carex_pilulifera',
#                                                                                                                                                                             ifelse(site == 'LYG' & plot == '3.2.' & species == 'Lotus_corniculatus' & year == '2018', 'Galium_saxatile',
#                                                                                                                                                                                    ifelse(site == 'LYG' & plot == '3.3.' & species == 'Luzula_multiflora_cf' & year == '2019', 'Luzula_multiflora',
#                                                                                                                                                                                           ifelse(site == 'LYG' & plot == '5.1.' & species == 'Carex_binervis' & year == '2018','Eriophorum_angustifolium',  
#                                                                                                                                                                                                  ifelse(site == 'LYG' & plot == '5.2.' & species == 'Carex_sp' & year == '2017','Eriophorum_angustifolium',
#                                                                                                                                                                                                         ifelse(site == 'LYG' & plot == '7.3.' & species == 'Eriophorum_vaginatum' & (year == '2017' | year == '2016'),'Eriophorum_angustifolium',  
#                                                                                                                                                                                                                ifelse(site == 'LYG' & plot == '9.2.' & species == 'Eriophorum_vaginatum' & year == '2019','Eriophorum_angustifolium',
#                                                                                                                                                                                                                       ifelse(site == 'LYG' & plot == '5.2.' & species == 'Carex_flava' & year == '2016','Carex_pilulifera',
#                                                                                                                                                                                                                              ifelse(site == 'LYG' & (plot == '5.2.' | plot == '5.3.') & species == 'Carex_nigra' & year == '2016', 'Carex_panicea',
#                                                                                                                                                                                                                                     ifelse(site == 'LYG' & (plot == '5.3.' | plot == '6.1.') & species == 'Carex_echinata' & year == '2016', 'Carex_pilulifera',
#                                                                                                                                                                                                                                            ifelse(site == 'LYG' & (plot == '5.2.' | plot == '5.3.') & species == 'Juncus_sp' & year == '2016', 'Juncus_squarrosus',
#                                                                                                                                                                                                                                                   ifelse(site == 'LYG' & plot == '7.1.' & (species == 'Carex_binervis' | species == 'Carex_pilulifera') & year == '2017', 'Carex_echinata',
#                                                                                                                                                                                                                                                          ifelse(site == 'GOL' & ((plot == '19.4.' |plot == '19.7.' | plot == '19.8.') &  species == 'Carex_sp' & year == '2016'), 'Carex_pilulifera', 
#                                                                                                                                                                                                                                                                 ifelse(site == 'GOL' & (plot == '19.8.' &  species == 'Festuca_sp' & year == '2017'), 'Festuca_rubra', 
#                                                                                                                                                                                                                                                                        ifelse(site == 'NOV' & (plot == '20.2.' &  species == '??' & year == '2018'), 'Unknown_herb',
#                                                                                                                                                                                                                                                                               ifelse(site == 'NOV' & (plot == '20.4.' &  species == 'Rhytidiadelphus_squarrosus' & year == '2017'), 'Rhytidiadelphus_loreus',
#                                                                                                                                                                                                                                                                                      ifelse(site == 'NOV' & (plot == '20.9.' &  species == 'Anthoxanthum_odoratum' & year == '2017'), 'Agrostis_capillaris',
#                                                                                                                                                                                                                                                                                             ifelse(site == 'NOV' & (plot == '20.9.' &  species == 'Carex_sp' & (year == '2016' | year == '2018')), 'Carex_pilulifera',
#                                                                                                                                                                                                                                                                                                    ifelse(site == 'NOV' & (plot == '20.9.' &  species == 'Rhytidiadelphus_loreus' & year == '2018'), 'Rhytidiadelphus_squarrosus',
#                                                                                                                                                                                                                                                                                                           species )))))))))))))))))))))))))))))))))))))))) %>% 
#   mutate ( species = new.species)  %>% 
#   group_by(site, plot, year, species, group, experiment) %>% 
#   summarise_at(.vars = "cover", sum, NA.rm = TRUE) 

)


##############################
# 
# 
# 
# community <- community.import %>% 
#   left_join(names, by= 'species') %>% 
#   mutate(species = coalesce(correct_name, species)) %>%  #jumps to second if first in NA, keep first if it is not NA
#   select(-correct_name) %>% 
#   mutate(plot = if_else(site == 'BUO' & year == '2016', #BUO had wrong plot numbers in 2016. Fix that
#                         true = recode(plot,   
#                                       '10.1.' = '11.1.',
#                                       '10.2.' = '10.1.',
#                                       '10.3.' = '12.1.',
#                                       '11.1.' = '10.3.',
#                                       '11.2.' = '10.2.',
#                                       '11.3.' = '11.2.',
#                                       '12.1.' = '12.2.',
#                                       '12.2.' = '11.3.',
#                                       '12.3.' = '12.3.'
#                         ), false = plot),
#          plot = str_replace(plot, pattern = "$" , replacement =  "."),       # This fix the plot number issue on TOR 
#          plot = str_replace(plot, pattern = "\\.\\.", replacement = ".")) %>% 
#   mutate(plot = if_else(site == 'SKO' & !year == '2017' , #SKO were misplaced with one in three years
#                         true = recode(plot,   
#                                       '25.2.' = '25.1.',
#                                       '25.3.' = '25.2.',
#                                       '25.4.' = '25.3.',
#                                       '25.5.' = '25.4.',
#                                       '25.6.' = '25.5.',
#                                       '25.7.' = '25.6.',
#                                       '25.8.' = '25.7.',
#                                       '25.9.' = '25.8.',
#                                       '25.10.' = '25.9.',
#                                       '25.0.' = '25.9.',
#                                       '25.1.' = '25.0.'  
#                         ),  false = plot)) %>% 
#   mutate(
#     new.species = case_when(site == 'BUO' & (species == 'Agrostis_canina' | species == 'Agrostis_capillaris' | species == 'Agrostis_sp') ~ 'Agrostis_capillaris',
#                             site == 'NOV' & species == 'Agrostis_canina', 'Agrostis_vinealis',
#                             TRUE ~ species))
#          
# 
#   mutate(new.species = coalesce(ifelse(site == 'BUO' & (species == 'Agrostis_canina' | species == 'Agrostis_capillaris' | species == 'Agrostis_sp'), 'Agrostis_capillaris', 
#                                        ifelse(site == 'NOV' & species == 'Agrostis_canina', 'Agrostis_vinealis', 
#                                               ifelse(site == 'GOL' & species == 'Lycopodium_annitonum', 'Lycopodum_clavatum', 
#                                                      ifelse(site == 'GOL' & (species == 'Polygala_vulgaris' | species == 'Polygala_sp'), 'Polygala_serpyllifolia', 
#                                                             ifelse(site == 'GOL' & species == 'Diphasiastrum_sp', 'Diphasiastrum_alpinum', 
#                                                                    ifelse((site == 'NOV' & species == 'Viola_sp') | (site == 'BUO' & (species == 'Viola_canina1' | species == 'Viola_sp' | species == 'Viola_riviniana')), 'Viola_canina',
#                                                                           ifelse(site == 'TOR' & species == 'Carex_binervis', 'Carex_vaginata',                                                                                     species ))))))))) %>% 
#   mutate ( species = new.species)
# 
# 

