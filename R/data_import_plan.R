## import and process data

import_plan <- drake_plan(
  
  #import meta data
  meta0 = read_xlsx(meta_download),
  
  #import environment
  env0 = read_xlsx(environment_download),
  
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
  calluna = read_xlsx(calluna_cover_download)
)

