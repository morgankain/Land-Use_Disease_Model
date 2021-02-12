###########################################################################
### Top level script for running landscape simulation. Loops over       ###
### diseases, landscapes, and the host of the initial infection         ###
###########################################################################

####
## Set up the key parameters for the simulation
####
source("parameters_top.R")

####
## Packages and functions
####
source("needed_packages.R")
source("ggplot_theme.R")
source("functions.R")

####
## Fit models to data from the lit for physiological responses of hosts and mosquitoes to infection
####
source("fit_models.R")

####
## Set up empty containers for all of the results
####
source("landscape_containers.R")

## Loop over chosen parameters for model runs. 
 ## Note: Each takes many hours (up to 12, so best advised to submit each as a job to a computing cluster) --- see folder "computation_strategy" for my strategy for submitting jobs
 ## Note: comment or un-comment the for( ) for multiple parameters or a single run
for (landscape_counter in seq_along(this_landscape_choice)) {
# for (landscape_counter in c(this_landscape_choice)) {
# landscape_counter <- 1 ## debugging 

for (disease_counter in seq_along(disease)) {
# disease_counter   <- 1 ## debugging 
  
if (disease[disease_counter] == "yellow_fever") {
  starting_host <- c("human", "primate")
} else {
  starting_host <-"human"
}
  
for (starting_host_counter in seq_along(starting_host)) {
# starting_host_counter <- 2 # starting_host_counter <- 1 ## debugging 

print(paste(
  "landscape =", landscape_counter, "|"
, "disease =", disease[disease_counter], "|"
, "starting host =", starting_host[starting_host_counter]
  ))
  
####
## Parameters for disease, mosquitoes, and hosts. Landscapes get simulated further down
####
source("parameters.R")
  
####
## For the given disease and set of mosquitoes, predict responses from the fitted models for the given disease and landscape
####
source("adj_models.R")
  
####
## Set up mosquito (and host) dispersal kernel
####
source("mosq_dispersal.R")

  if (sim_landscape) {
####
## Simulate a landscape using NLMR to simulate Random Fractal Maps
####
source("sim_landscapes.R")
  } else {
####
## Alternatively, bring in a real landscape and parse into the landscape array that the sim needs
####
source("convert_landscapes.R")
## To simulate scenarios on the empirical landscapes. Run a single time and output saved for loading
# source("convert_landscapes_sim.R")
  }
  
####
## Transform the landscape into populations of "other" hosts and mosquitoes
####
source("pop_dens.R")
  
####
## Finally run the simulation (SLOW -- e.g. 6-12h, be warned, to text model use extra_reduce = T which will run in a few mintues)
####
source("run_sim.R")

## Save a temporary product at each parameter combination
temp_nam <- paste(
 paste(
  paste("saved_output/landscape.out", Sys.Date(), sep = "_")
, paste(
  sim_landscape
, dens_run
, this_landscape_choice
, travel_dist[1]
, pop_exp
, sep = "_")
, sep = "_"
)
, ".Rds", sep = ""
)
  
try(saveRDS(landscape.out.top, temp_nam), silent = TRUE)
  
  }
 }
}

## At the end save final results
temp_nam <- paste(
 paste(
  paste("saved_output/landscape.out", Sys.Date(), sep = "_")
, paste(
  sim_landscape
, dens_run
, this_landscape_choice
, travel_dist[1]
, pop_exp
, sep = "_")
, "Final"
, sep = "_"
)
, ".Rds", sep = ""
)
  
try(saveRDS(landscape.out.top, temp_nam), silent = TRUE)

####
## Examine results outside of the loop using these scripts. 
## Do not use source, as that will not work. These scripts were set up to parse the output I received from all of my model runs on the
## computing cluster. Some individual plots will work that use individual model output though. The code below can be used for ideas on 
## how to summarize output and plot results, however. See commenting within individual scripts for details and please contact me with questions
####

## "explore_results.R"             ## Summarize and organize multiple saved output Rds
## "manuscript_figure_details.R"   ## Details on the parameter values used in simulations to obtain the output for each manuscript figure
## "manuscript_figures.R"          ## Figure plotting used summarized output created in "explore_results.R"
## "manuscript_supp_figures.R"     ## Supplementfigure plotting used summarized output created in "explore_results.R"
