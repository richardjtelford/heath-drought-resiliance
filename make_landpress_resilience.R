#load packages
library("drake")

## DO THIS FIRST
# Generate an osf personal access token at
# https://osf.io/settings/tokens/create
# Run
# usethis::edit_r_environ()
# and add the line 
# OSF_PAT=my_70_character_token

#plan
resilience_plan <- "R/1_main_resilience_drake_plan.R"

#Build the right things
r_make(source = resilience_plan)
drake_failed()

# show methods results
if(length(drake_failed()) == 0){
  fs::file_show("methods_results.pdf")#display pdf
}


#view dependency graph
r_vis_drake_graph(source = resilience_plan, targets_only = TRUE)
