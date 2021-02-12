####################################################################
## Obtain simulated landscapes across a gradient of heterogeneity ##
####################################################################

## Thee goal here is to simulate two landscapes (one for urban area and one for tree cover) with different levels of fragmentation (varying degrees of spatial auto-correlation) 
 ## such that on each landscape there is the same amount of overlap between tree cover and urban area (same amount of negative correlation). 
  ## Couldn't find a way to do this automatically with any canned package, so this script combines the simulation of individual landscapes with a brute force rejection of configurations 
   ## that fail certain checks (e.g., not an appropriate value of negative correlation)

## Getting landscapes in this way is a very slow process so this script is set up to allow for saving and reloading of previously simulated landscapes 

## Just a bit of setup
landscape_predictors <- c("human_population", "tree_cover")
num_land             <- length(landscape_predictors)
which_tree           <- grep("tree", landscape_predictors)

## Landscape matrix size that is reasonable for computation
matx      <- 60
maty      <- 60

## values of spatial heterogeneity
het       <- seq(0, 0.92, by = 0.02)

## range of correlation between the two landscape attributes that will be accepted for these landscapes 
 ## Note: the stronger the negative correlation and the narrower the window the longer the simulation will take. 
  ## Settled on this range as an acceptable non-overlap that would actually succeed in finding landscapes 
corrange    <- c(-0.55, -0.45)
## For each landscape also force the average value on the landscape to be near 0.5. Scaling for human and other host densities happens from the simulated 0-1 values
 ## that have an average across the landscape of close to 0.5 
meanrange   <- c(0.48, 0.52)

## Include parameter values in the name of the saved file otherwise this could get confusing in the future
landname <- paste(
  "saved_input/"
, paste(
  paste("cor",  paste(corrange, collapse = "_"), sep = "_")
, paste("mean", paste(meanrange, collapse = "_"), sep = "_")
#, paste("mean_1", paste(meanrange_1, collapse = "_"), sep = "_")
#, paste("mean_2", paste(meanrange_2, collapse = "_"), sep = "_")
, paste("dim",  paste(matx, collapse = "_"), sep = "_")
, sep = "_")
, "_sim_land.Rds"
, sep = "")

if (file.exists(landname)) {
sim_land <- readRDS(landname)
} else {
sim_land  <- vector("list", nsim) 
## Need these to simulate new landscapes, but if just using previously simulated landscapes don't need load them
library(NLMR); library(raster) # library(landscapetools)
}

for (i in seq_along(het)) {
  
## load a previously successful simulated landscape at this 
if (is.null(sim_land[[i]])) {
  
## just filling for now with data to get large cor
land1 <- nlm_mpd(ncol = matx, nrow = maty, roughness = 0.01, rand_dev = 10)
land2 <- nlm_mpd(ncol = matx, nrow = maty, roughness = 0.01, rand_dev = 10)

## Calc correlation between the landscapes
tempcor   <- cor(c(land1@data@values), c(land2@data@values))
## Aiming for the sweetspot of corrange. When inbetween this will turn up FALSE
testcor   <- sum(((tempcor < corrange[1]) | (tempcor > corrange[2])))

## Also check the mean of the pixel values for each landscape. Want these to be near 0.5 so that the values span the full range
tempmeans <- matrix(data = c(mean(c(land1@data@values)), mean(c(land2@data@values))), nrow = 1)
## Aiming for the sweetspot of meanrange. When both are inbetween this will turn up FALSE, FALSE
testmeans <- sum(apply(tempmeans, 2, FUN = function (x) ((x < meanrange[1]) | (x > meanrange[2])))) 
#testmeans <- ((tempmeans[1,1] < meanrange_1[1]) | (tempmeans[1,1] > meanrange_1[2])) +
#  ((tempmeans[1,2] < meanrange_2[1]) | (tempmeans[1,2] > meanrange_2[2]))
  
## simulate landscapes until testcor and testmeans is satisfied
while (sum(testcor, testmeans) != 0) {

land1 <- nlm_mpd(ncol = matx, nrow = maty, roughness = het[i], rand_dev = 10, rescale = TRUE)
land2 <- nlm_mpd(ncol = matx, nrow = maty, roughness = het[i], rand_dev = 10, rescale = TRUE)
  
## checks for these simulated landscapes
tempcor   <- cor(c(land1@data@values), c(land2@data@values)); tempcor
testcor   <- sum(((tempcor < corrange[1]) | (tempcor > corrange[2])))
tempmeans <- matrix(data = c(mean(c(land1@data@values)), mean(c(land2@data@values))), nrow = 1)
testmeans <- sum(apply(tempmeans, 2, FUN = function (x) ((x < meanrange[1]) | (x > meanrange[2]))))
#testmeans <- ((tempmeans[1,1] < meanrange_1[1]) | (tempmeans[1,1] > meanrange_1[2])) +
#  ((tempmeans[1,2] < meanrange_2[1]) | (tempmeans[1,2] > meanrange_2[2]))
  
}
  
## extract just the landscape values
land1 <- matrix(data = land1@data@values, nrow = matx - 1, ncol = matx - 1)
land2 <- matrix(data = land2@data@values, nrow = matx - 1, ncol = matx - 1)
  
## store in the list as a successful landscape combo
temp_array      <- array(data = 0, dim = c(matx - 1, maty - 1, 2))
temp_array[,,1] <- land1
temp_array[,,2] <- land2
dimnames(temp_array)[[3]] <- landscape_predictors

sim_land[[i]]        <- temp_array

## And rewrite the landscape so that this landscape doesn't need to be simulated again
saveRDS(sim_land, landname)

}

## make sure i = i + 1 fulfills while loop requirements
testcor <- 1
#print(het[i])
  
}

## Use the sim landscape and fill out the rest of this landsape entry
landscape[[landscape_counter]] <- array(
    data = 0
  , dim = c(
     matx - 1
   , maty - 1
   , 2 + num_hosts + num_mosq
  ))

if (disease[disease_counter] != "yellow_fever") {
dimnames(landscape[[landscape_counter]]) <- list(
  NULL
, NULL
, c("urban", "human", "forest", "other", mosq_names)
)
} else {
dimnames(landscape[[landscape_counter]]) <- list(
  NULL
, NULL
, c("urban", "human", "forest", "primate", "other", mosq_names)
)  
}

## A few final needed scaling steps

## Just the current one
current_landscape <- sim_land[[which_landscapes[landscape_counter]]]

## First, downstream breaks if there are true 0 trees or true 0 humans. I should fix
 ## downstream how mosquito distributions are calculated and allow 0s, but for now it breaks
  ## (see mosq_coverage calculation in R0_internal_calc_fast_opaque.R)
zero_vals.h <- which(current_landscape[,,landscape_predictors[1]] == 0, arr.ind = T)
nz_min.h    <- min(current_landscape[,,landscape_predictors[1]][current_landscape[,,landscape_predictors[1]] != 0])
for (zf in 1:nrow(zero_vals.h)) {
 current_landscape[,,landscape_predictors[1]][zero_vals.h[zf, 1], zero_vals.h[zf, 2]] <- 
 nz_min.h
}

zero_vals.t <- which(current_landscape[,,landscape_predictors[2]] == 0, arr.ind = T)
nz_min.t    <- min(current_landscape[,,landscape_predictors[2]][current_landscape[,,landscape_predictors[2]] != 0])
for (zf in 1:nrow(zero_vals.t)) {
 current_landscape[,,landscape_predictors[2]][zero_vals.t[zf, 1], zero_vals.t[zf, 2]] <- 
 nz_min.t
}

## old names to new names
landscape[[landscape_counter]][,,"urban"]  <- current_landscape[,,landscape_predictors[1]]
landscape[[landscape_counter]][,,"forest"] <- current_landscape[,,landscape_predictors[2]]
