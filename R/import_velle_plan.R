#TODO 
#1 Hypn sp....37 
#2 Hypn sp....135

import_velle_plan <- drake_plan(
  
  velle_species1 = read_xlsx(
    path = "data/Species subscribed to groups_succession_increase_decrease_22.10.2020.xlsx",
    sheet = "Sheet1") %>% 
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
    mutate(
      cover = if_else(is.na(cover), true = 0, false = cover),
      percent = case_when(
        cover == 0 ~ 0,
        cover == 2 ~ (0 + 1)/2,
        cover == 3 ~ (1.0 + 3.125)/2,
        cover == 4 ~ (3.125 + 6.25)/2,
        cover == 5 ~ (6.25 + 12.5)/2,
        cover == 6 ~ (12.5 + 25.0)/2,
        cover == 7 ~ (25 + 50)/2,
        cover == 8 ~ (50 + 75)/2,
        cover == 9 ~ (75 + 100)/2, 
        TRUE ~ NA_real_ #unexpected value
      )
    ) %>%
   left_join(velle_species, by = "Abbreviation")
)

# velle_community_long %>%
#   group_by(site, year, habitat, Group.x,  block, plot) %>% 
#   summarise(percent = sum(percent)) %>%
#   group_by(site, year, Group.x) %>% 
#   summarise(percent = mean(percent)) %>%
#   ggplot(aes(x = year, y = percent, fill = Group.x)) +
#   geom_col() + 
#   scale_fill_brewer(palette = "Dark2") +
#   facet_wrap(~ site, ncol = 1, strip.position = "right") +
#   labs(x = "Year", y = "Cover %", fill = "Functional Group") +
#   theme(strip.text.y = element_text(angle = 0))


  

