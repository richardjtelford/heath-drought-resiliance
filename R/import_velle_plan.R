#TODO 
#1 Hypn sp....37 
#2 Hypn sp....135

import_velle_plan <- drake_plan(
  
  velle_species1 = read_xlsx(
    path = "data/Species subscribed to groups_LP_SVH_LGV_14.10.2020.xlsx",
    sheet = "Sheet1",
    col_types = c(rep("text", 3), "skip", "text")) %>% 
    rename_with(~str_replace(.x, " ", "_")) %>%
    rename(Latin = Latin_name) %>% 
    mutate(Latin = str_replace(Latin, "_", " ")),
  
  velle_species2 = read_xlsx(
    path = "data/Subscribing species names to groupds_PhD_LGV_14.10.2020.xlsx",
    sheet = "Ark1") %>% 
    rename_with(~str_replace(.x, " ", "_")),
  
  velle_species = velle_species2 %>% 
    left_join(velle_species1, by = "Latin"),
  
  velle_community = read_xlsx(
      path = "data/Data LG Velle PhD_8.10.2020.xlsx", 
      sheet = "Species cover"
    ) %>% 
    extract(
      col = Nr, 
      into = c("site", "habitat", "treatment", "block", "plot", "year"), 
      regex = "(\\w)(\\w)(\\w)-(\\d)(\\d)(\\d)", 
      convert = TRUE
    ),
      
 velle_cover = read_xlsx(
   path = "data/Data LG Velle PhD_8.10.2020.xlsx", 
   sheet = "Cover layers, seedlings") %>% 
   extract(
     col = Nr, 
     into = c("site", "habitat", "treatment", "block", "plot", "year"), 
     regex = "(\\w)(\\w)(\\w)-(\\d)(\\d)(\\d)",
     convert = TRUE
   ),
 
  velle_community_long = velle_community %>% 
    pivot_longer(
      cols = -c("site", "habitat", "treatment", "block", "plot", "year"),
      names_to = "Abbreviation", 
      values_to = "cover") %>% 
    left_join(velle_species, by = "Abbreviation")
)



  

