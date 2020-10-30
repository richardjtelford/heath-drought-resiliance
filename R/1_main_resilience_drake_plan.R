#trait driver theory

#load packages
library("drake")
library("tidyverse")
library("readxl")
library("broom")
library("rjt.misc")
library("dataDownloader")
library("nlme")
library("vegan")
library("ggvegan")
library("patchwork")
library("mapdata")
library("ggrepel")
library("lubridate")
library("conflicted")
library("ggnewscale")
conflict_prefer("filter", winner = "dplyr")
conflict_prefer("select", winner = "dplyr")
conflict_prefer("map", winner = "purrr")


#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#source sub-plans
source("R/download_plan.R")
source("R/data_import_plan.R")
source("R/community_cleaning_plan.R")
source("R/figures_plan.R")
source("R/ordination_plan.R")
source("R/import_velle_plan.R")
source("R/damage_plan.R")
source("R/weather_plan.R")

#source extra function


#drake plan
analysis_plan <- drake_plan(
  
)

# manuscript plan
manuscript_plan <- drake_plan(
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("vegan", "drake", "tidyverse", "rmarkdown", "renv"), 
    old_bib = file_in("Rmd/TDT.bib"), 
    new_bib = file_out("Rmd/TDT2.bib")),
  
  #knit manuscript
  manuscript = {
    file_in("Rmd/elsevier-harvard_rjt.csl")
    file_in("Rmd/TDT2.bib")
    rmarkdown::render(
      input = knitr_in("methods_results.Rmd"), 
      clean = FALSE)
  }
)

#### combine plans ####
trait_plan <- bind_plans(download_plan, 
                        import_plan,
                        import_velle_plan,
                        clean_community_plan,
                        figures_plan,
                        ordination_plan,
                        damage_plan,
                        weather_plan,
                        manuscript_plan)
#quick plot
plot(trait_plan)

#### configure drake plan ####
trait_config <- drake_config(plan = trait_plan, keep_going = TRUE)
trait_config
