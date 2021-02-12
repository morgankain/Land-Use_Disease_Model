#######################################################
## Simulate reforestation on the empirical landscape ##
#######################################################

## Use technique and structure from sim_landscapes.R

corrange    <- c(-0.55, -0.45)
meanrange   <- c(0.48, 0.52)
het         <- c(0.3, 0.7)  ## goal here is to create a large contiguous forest and a highly patchy forest
## the last option will be to simply upscale all pixels to get to the mean

landname <- paste(
  "saved_input/"
, paste(
  "empirical_land"
, sep = "_")
, "_sim_land.Rds"
, sep = "")

if (file.exists(landname)) {
sim_land <- readRDS(landname)
} else {
sim_land <- vector("list", length(het) + 1)  
}

for (i in seq_along(het)) {
  
## load a previously successful simulated landscape at this 
if (is.null(sim_land[[i]])) {
  
## use the actual background population density unchanged and adjust the tree cover while still maintaining the negative correlation
pop.default    <- get(paste("land", landscape_counter, ".", "pop", sep = "")) 
forest.default <- get(paste("land", landscape_counter, ".", "lai", sep = ""))
forest.adj     <- nlm_mpd(ncol = max(maty, matx) + 2, nrow = max(maty, matx) + 2, roughness = 0.01, rand_dev = 10)
forest.adj     <- matrix(forest.adj@data@values, nrow = forest.adj@nrows, ncol = forest.adj@ncols)
forest.adj     <- forest.adj[1:matx, 1:maty]
forest.final   <- forest.default + forest.adj

## Calc correlation between the landscapes
tempcor   <- cor(c(pop.default), c(forest.final))
## Aiming for the sweetspot of corrange. When inbetween this will turn up FALSE
testcor   <- sum(((tempcor < corrange[1]) | (tempcor > corrange[2])))

## Also check the mean of the pixel values for each landscape. Want these to be near 0.5 so that the values
 ## span the full range
tempmeans <- matrix(data = mean(c(forest.final)), nrow = 1)
## Aiming for the sweetspot of meanrange. When both are inbetween this will turn up FALSE, FALSE
testmeans <- sum(apply(tempmeans, 2, FUN = function (x) ((x < meanrange[1]) | (x > meanrange[2])))) 

i_try      <- 0
i_try_cor  <- numeric(length = 0)
i_try_mean <- numeric(length = 0)

## simulate landscapes until testcor and testmeans is satisfied
while (testcor != 0) {

forest.adj     <- nlm_mpd(ncol = max(maty, matx) + 2, nrow = max(maty, matx) + 2, roughness = het[i], rand_dev = 10)
forest.adj     <- matrix(forest.adj@data@values, nrow = forest.adj@nrows, ncol = forest.adj@ncols)
forest.adj     <- forest.adj[1:matx, 1:maty]
forest.final   <- forest.default + forest.adj
forest.final   <- forest.final / max(forest.final)
  
## checks for these simulated landscapes
tempcor    <- cor(c(pop.default), c(forest.final))
i_try_cor  <- c(i_try_cor, tempcor)
testcor    <- sum(((tempcor < corrange[1]) | (tempcor > corrange[2])))
tempmeans  <- matrix(data = mean(c(forest.final)), nrow = 1)
i_try_mean <- c(i_try_mean, tempmeans)
testmeans  <- sum(apply(tempmeans, 2, FUN = function (x) ((x < meanrange[1]) | (x > meanrange[2]))))

i_try      <- i_try + 1

if (((i_try / 20) %% 1) == 0) { print (i_try) }
  
}
  
## store in the list as a successful landscape combo
sim_land[[i]]  <- forest.final

}

testcor <- 1
print(i)
  
}

## Manual option to up-adjust all pixels to reach the target mean forest cover
sim_land[[3]] <- forest.default * (0.5/mean(forest.default))
sim_land[[1]] <- sim_land[[1]]  * (0.5/mean(sim_land[[1]]))
sim_land[[2]] <- sim_land[[2]]  * (0.5/mean(sim_land[[2]]))

saveRDS(sim_land, landname)
