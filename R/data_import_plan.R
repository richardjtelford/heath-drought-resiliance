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
      across(everything(), ~str_replace(.x, "^+$", "1")), #change "+" to "1"
      across(everything(), ~if_else(.x == "N", NA_character_, .x)), #change "N" to NA
    ) %>% 
    #convert to numeric
    #will lose ~100 non-numeric values at the moment
    mutate(across(max_height1:cover_bryophytes, as.numeric))
)

