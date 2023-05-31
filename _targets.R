# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "readxl", "dataDownloader", "patchwork", "rjt.misc", "vegan", "ggvegan", "pipebind", "lme4"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)


# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

## globals
remote_path <- "LandPress_Data_for_resiliencepaper/Resilience (temporary)"
check_update <- FALSE

# Replace the target list below with your own:
list(
  # download data
  tar_target(
    name = meta_download,
    command =  get_file(
      node = "mv84d",
      remote_path = remote_path,
      file = "drought.plots.xlsx",
      path = "data"
    ), 
    format = "file",
    cue = tar_cue_force(condition = 
                          check_update && dataDownloader::need_update(
        node = "mv84d",
        remote_path = remote_path,
        file = "drought.plots.xlsx",
        path = "data"
      ))
  ),
  #environment
 tar_target(
   name =  environment_download,
   command =  get_file(
      node = "mv84d",
      remote_path = remote_path,
      file = "Environmental variables resilience_2020.xlsx",
      path = "data"
    ),
    format = "file",
    cue = tar_cue_force(
      check_update && dataDownloader::need_update(
        node = "mv84d",
        remote_path = remote_path,
        file = "Environmental variables resilience_2020.xlsx",
        path = "data"
      )
    )
  ),
  
  #community data
   tar_target(
     name = community_download,
   command = get_file(
      node = "mv84d",
      remote_path = remote_path,
      file = "DE.1_Community.xlsx",
      path = "data"
    ),
    format = "file",
    cue = tar_cue_force(
      check_update && dataDownloader::need_update(
        node = "mv84d",
        remote_path = remote_path,
        file = "DE.1_Community.xlsx",
        path = "data"
      )
    )
  ),
  
  #calluna cover data
  tar_target(
     name = calluna_cover_download,
    command = get_file(
      node = "mv84d",
      remote_path = remote_path,
      file = "Datasett resiliens med endringer LGV_29.6.2020.xlsx",
      path = "data"
    ),
    format = "file",
    cue = tar_cue_force(
      check_update && dataDownloader::need_update(
        node = "mv84d",
        remote_path = remote_path,
        file = "Datasett resiliens med endringer LGV_29.6.2020.xlsx",
        path = "data"
      )
    )
  ),

  # import data
 tar_target(
   name = meta0,
   command = load_metadata(meta_download)
 ),
 tar_target(
   name = env0,
   command = load_environment(environment_download)
 ),
 tar_target(
   name = comm0,
   command = load_community(community_download)
 ),
 tar_target(
   name = spp_names,
   command = load_spp_names(community_download)
 ),
 tar_target(
   name = calluna_cover,
   command = load_calluna_cover(calluna_cover_download)
 ),
 tar_target(
   name = seedlings,
   command = load_seedlings(calluna_cover_download, meta0)
 ),
 
  tar_target(
    name = all_covers,
    command = load_all_covers(calluna_cover_download)
  ),
 
 tar_target(
   name = site_data, 
   command = make_site_data(comm)
 ),
 
 # clean data
 tar_target(
   name = comm, 
   command = clean_comm(comm0, spp_names, calluna_cover, meta0)
 ),
 
 # default colour scheme
 tar_target(
   name = site_colours,
   command = scale_colour_viridis_d(option = "B", end = 0.9)
 ),
  tar_target(
    name = site_fill, 
    command = scale_fill_viridis_d(option = "B", end = 0.9)
  ),
 # climate data/plots
 tar_target(
   name = weather_data,
   command = "data/Resiliens_climate_jan_feb.xlsx",
   format = "file"
 ),
 tar_target(
   name = weather, 
   command = load_weather(weather_data)
 ),
 tar_target(
   name = weather_plot,
   command = make_weather_plot(weather, site_colours)
 ),
 tar_target(
   name = climate_data,
   command = "data/Nedbør.xlsx",
   format = "file"
 ),
 tar_target(
   name = climate, 
   command = load_climate(climate_data)
 ),
 tar_target(
   name = climate_plot, 
   command = make_climate_plot(climate, site_fill)
 ),
 
 

 # make figures
 tar_target(
   name = map,
   command = make_map(site_data)
 ),
 tar_target(
   name = calluna_cover_plot,
   command = make_calluna_cover_plot(calluna_cover, site_data)
 ),
 tar_target(
   name = community_group_cover_plot,
   command = make_community_group_cover_plot(comm, meta0, site_data)
 ),
 tar_target(
   name = seedling_plot,
   command = make_seedling_plot(seedlings, meta0, site_data, site_colours)
 ),
 
 # run analyses
 tar_target(
   name = seedling_model,
   command = make_seedling_model(seedlings, meta0, site_data)
 ),
 tar_target(
   name = calluna_recovery_model,
   command = make_calluna_models(calluna_cover)
 ),
 
 
 # ordinations
 tar_target(
   name = spp_summ,
   command = make_spp_summ(comm)
 ),
 tar_target(
   name = comm_wide,
   command = make_wide_comm(comm, meta0, site_data)
 ),
 tar_target(
   name = nmds_plots,
   command = make_nmds_plots(comm_wide, comm, spp_names, spp_summ, site_colours)
 ),
 tar_target(
   name = pca_plots,
   command = make_pca_plots(comm_wide, spp_names, spp_summ, site_colours)
 ),
 tar_target(
   name = ca_plots,
   command = make_ca_plots(comm_wide, spp_names, spp_summ, site_colours)
 ),
 
 # damage by enviroment
 tar_target(
   name = damage_model_data,
   command = make_damage_model_data(calluna_cover, site_data, env0)
 ),
 tar_target(
   name = dead_damaged_plots,
   command = make_dead_damaged_plots(damage_model_data, site_colours)
 ),
   

 # manuscript
 tar_target(
   name = biblio,
   command = "Rmd/TDT.bib",
   format = "file"
 ),
 tar_target(
   name = biblio2,
   #add extra packages to bibliography
   command = package_citations(
     packages = c("vegan", "targets", "tidyverse", "quarto", "renv", "lme4", "lmerTest"), 
     old_bib = biblio, 
     new_bib = "Rmd/TDT2.bib"),
   format = "file"
 ),
   
   #knit manuscript
   tar_quarto(
     name = manuscript,
     path = "methods_results.qmd",
     extra_files = c("Rmd/elsevier-harvard_rjt.csl", "Rmd/TDT2.bib")
 )
 
)
