##################################################
## Set up the key parameters for the simulation ##
##################################################

## Pick the diseases to measure risk for (the diseases for which we want to measure tradeoffs). These are the only diseases supported. 
disease          <- c("dengue", "yellow_fever", "malaria")

## 1-10 (10 different levels of spatial auto-correlation used for the landscape configurations) 
 ## 1 equates to the highest spatial auto-correlation, 10 the highest -- See both landscape_containers.R and sim_landscapes.R for details
this_landscape_choice <- 1 

## Scaling of urban intensity to human population size --- up-scaling of urban intensity (0-1) to the number of humans per pixel (urban intensity * human_scaling)
 ## Note: with landscape pixels of width and length of 200m--which is 4 hectares--(which is what we use in the manuscript), using 100 for scaling would give 
  ## 25 people/hectare. For reference most neighborhoods in NYC have about 350 people/hectare
dens_run              <- 3 ## 1-7  (7 possible values for the abundance humans and other on the landscape)
human_scaling         <- rev(c(5, 10, 20, 40, 80, 160, 320))[dens_run]

## Exponent for mosquito dependence on hosts  -- See parameters.R for details
pop_exp               <- 1 

####
## Additional parameter values for simulations
####
sim_landscape    <- TRUE   ## Simulate landscape or pull in external "empirical" landscape? --- See sim_landscape.R for details
extra_reduce     <- FALSE  ## Run a tiny subset of the run for debugging?
host_movement    <- TRUE   ## Allow movement of infected host?
boxed_in         <- TRUE   ## "trap" hosts and mosquitoes on the landscape?
                            ## (treat edge effects by assuming a host or mosquito at the edge of the landscape disperses back into the landscape)
                             ## Preserves the dispersal distribution to sum to 1, but will produce some oddities on the edges of the landscape (which we slice off for manuscript figures)
start_scaling    <- "flat" ## Weight the emergence location of infection (for supplemental figures)
                            ## flat     = no scaling; assume an infection arises in each cell, so the FOI is calculated by summming the contribution of X by Y source infections (used in the main text)
                            ## weighted = scale emergence probability by some scaling factor (these could be many different sensible choices here): 
                              ## dengue by human pop, yellow fever by primate pop, malaria by anopheles pop (again, could be a number of options here --- just used for supp for the manuscript)
                            ## single   = model a _single_ infection arising on the landscape weighted in a similar way as the above ^^. With this scaling FOI will
                              ## be extremely tiny, but useful to determine relative risk

