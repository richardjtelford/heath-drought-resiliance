## import and process data

import_plan <- drake_plan(
  
  #import meta data
  meta0 = read_xlsx(meta_download),
  
  #import environment
  env0 = read_xlsx(environment_download, na = "na"),
  
  #import community
  comm0 = read_xlsx(community_download, 
                   sheet = 'frequency', 
                   col_types = 'text', 
                   na = 'NA') %>% 
    mutate(cover = recode(cover, '.+' = '1'),
           cover = as.numeric(cover)),
  
  spp_names = read_xlsx(community_download, sheet = 'corr_names') %>% 
    filter(!(is.na(correct_name))),
    
  #import calluna cover
  calluna_cover = read_xlsx(calluna_cover_download, sheet = "Ferdigstilling", na = "na"), 
    
  all_covers = read_xlsx(calluna_cover_download, sheet = "cover", na = "NA") %>% 
    #fix plot codes
    mutate(
      #add dot at end
      plot = str_replace(plot, "^(\\d{1,2}\\.\\d{1,2})$", "\\1."),
      #fix remaining problems
      plot = case_when(
        lokalitet == "TOR" & year == 2018 & plot == 14 ~ "24.14.",
        lokalitet == "TOR" & year == 2018 & plot == 16 ~ "24.16.",
        lokalitet == "ROS" & year == 2019 & plot == "22.10." ~ "22.0.",
        lokalitet == "SKO" & year == 2019 & plot == "25.10." ~ "25.0.",
        TRUE ~ plot
      )
    ) %>% 
    #fix non-numeric values in data
    mutate(
      across(everything(), ~str_replace(.x, ",", ".")), #change "," to "."
      across(everything(), ~str_replace(.x, ".$", "")), #fix stray "." after number
      across(everything(), ~str_replace(.x, "^+$", "1")), #change "+" to "1"
      across(everything(), ~if_else(.x == "N", NA_character_, .x)), #change "N" to NA
    ) %>% 
    #convert to numeric
    #will lose ~100 non-numeric values at the moment
    mutate(across(max_height1:cover_bryophytes, as.numeric)),
  
  site_data = read_delim("Site\tSite name	Mean annual precipitation (mm)	Mean January temperature (C)	Mean July temperature (C)	Biogeographic section (Moen, 1998)	Latitude	Longitude
BER	Bergsnova	1535			O2	64.841056	10.848461
BUO	Store Buøya	1132			O1	65.83677	12.224506
GOL	Golta	2080			O2	60.220656	5.001823
HAV	Haverøya	1136			O1	64.779	11.2193
LYG	Lygra	3209			O2	60.70084	5.092566
NOV	Novelandet	2673			O2	61.807887	4.922439
ROS	Rossvolløya	1136			O2	63.304347	8.007803
SKO	Skotsvær	1309			O1	65.79602	12.219299
TOR	Torsøya	1131			O2	65.68843	12.067688
YTS	Ytstevika	2560			O2	62.359774	5.519645", delim = "\t") %>% 
    arrange(desc(Latitude)) %>% 
    mutate(code = letters[1:nrow(.)])
)

