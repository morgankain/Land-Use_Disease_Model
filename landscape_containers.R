########################################
## Set up containers for model output ##
########################################

## number of landscapes to consider (get simulated or loaded later, but specify up front)
if (sim_landscape) {
nsim             <- 47                       ## Number of pre-simulated landscapes --- see sim_landscapes.R for details
which_landscapes <- seq(2, nsim, by = 5)     ## Too computationally intensive to run the model over every possible heterogeneity value
                                              ## 1:10 for this_landscape_choice correspond to jumps of 5 in the list of 47 simulated landscapes
num_landscapes   <- length(which_landscapes)
} else {
## 2 locations, each has baseline + 3 simulated scenarios of reforestation
num_landscapes   <- 2 * 4
}

## top level results containers (two stages because of non-human only showing up as a starting host for yellow fever)
starting_host            <- c("human")
landscape                <- vector("list", num_landscapes) ## container for all of the simulated landscapes
landscape.out.top        <- vector("list", num_landscapes * length(disease) * length(starting_host))
names(landscape.out.top) <- paste(
    rep(disease, each = num_landscapes * length(starting_host))
  , rep(seq(1:num_landscapes), length(starting_host))
  , rep(rep(starting_host, each = num_landscapes), length(disease))
  , sep = "_")

landscape.out.top2        <- vector("list", num_landscapes * 1 * 1)
names(landscape.out.top2) <- paste(
    rep("yellow_fever", each = num_landscapes * 1)
  , rep(seq(1:num_landscapes), 1)
  , rep(rep("primate", each = num_landscapes), 1)
  , sep = "_")

landscape.out.top <- c(landscape.out.top, landscape.out.top2)
