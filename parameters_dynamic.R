##########################################################################
### For every given cell that the infected host enters determine which ###
### mosquitoes are there and where they come from                      ###
##########################################################################

## Assumption here is that each mosquito originating in a given location will spend its time in the
 ## surrounding area following a Gaussian spatial kernel, with sd defined by flight distance, 
  ## and if desired, weighted by their preference for a given habitat type

## For every cell i3, j3 where and infection originates (which we are looping over in the internal loop of run_sim_kernel.R)
 ## need to apply the weighting matrix filter
mosq_temp_mosq_layers             <- vector("list", num_mosq)
mosq_temp_mosq_layers.weighting   <- vector("list", num_mosq)
mosq_temp_mosq_layers.coord.home  <- vector("list", num_mosq)
mosq_temp_mosq_layers.coord.all   <- vector("list", num_mosq)
mosq_temp_mosq_layers.coord.range <- vector("list", num_mosq)

## first check how much time the hosts spends in grid cell i3, j3 given its movement
host_time.now <- temp_weight.h[i3, j3]
  
for (k in 1:num_mosq) {
  
## Find all mosquito "home bases" that are close enough to send mosquitoes into i3, j3 (given flight distance of mosquito k)
 ## Identical code to figure out the mosquitoes range as to find the hosts range used one script level up (run_sim_kernel.R)
temp_dim_i  <- (temp_dim_i.h[i3] - mosq_dim[k]):(temp_dim_i.h[i3] + mosq_dim[k])
temp_dim_j  <- (temp_dim_j.h[j3] - mosq_dim[k]):(temp_dim_j.h[j3] + mosq_dim[k])
which_off_i <- which((temp_dim_i < 1) | (temp_dim_i > dim(landscape[[landscape_counter]])[1]))
which_off_j <- which((temp_dim_j < 1) | (temp_dim_j > dim(landscape[[landscape_counter]])[2]))
which_i     <- seq(1, length(temp_dim_i))
which_j     <- seq(1, length(temp_dim_j))
if (length(which_off_i) > 0) {
temp_dim_i  <- temp_dim_i[-which_off_i]
which_i     <- which_i[-which_off_i]
}
if (length(which_off_j) > 0) {
temp_dim_j  <- temp_dim_j[-which_off_j]
which_j     <- which_j[-which_off_j]
}
  
## For i3, j3 keep track of mosquitoes from each home base around i3, j3 that sends any number of mosquitoes into focal cell i3, j3

## The dimension of cells around which mosquitoes are deposited into i3, j3 is defined by the flight distance
 ## Cells with center mosq_dim away will send mosquitoes into i3, j3
mosq_temp_mosq_layers[[k]] <- array(data = 0, dim = c(
  dim(landscape[[landscape_counter]])[1]
, dim(landscape[[landscape_counter]])[2]
, length(which_i) * length(which_j)
  )
)

mosq_temp_mosq_layers.coord.all[[k]] <- array(data = 0, dim = c(
  dim(landscape[[landscape_counter]])[1]
, dim(landscape[[landscape_counter]])[2]
, length(which_i) * length(which_j)
  )
)


mosq_temp_mosq_layers.weighting[[k]]  <- vector("list", length(which_i) * length(which_j))

## Coordinates of all of the home base mosquito cells 
mosq_temp_mosq_layers.coord.home[[k]] <- array(data = 0, dim = c(
  1
, 2
, length(which_i) * length(which_j)
  )
  )

mosq_temp_mosq_layers.coord.range[[k]] <- vector("list", length(which_i) * length(which_j))

p = 1

## Loop over the spatial distribution of "home base" pixels that send mosquitoes into the grid cell where the infected host currently resides (i3, j3)
for (i2 in seq_along(temp_dim_i)) {
  for (j2 in seq_along(temp_dim_j)) {

## Find the distribution of mosquitoes in home spot temp_dim_i[i2], temp_dim_j[j2]
temp_dim_i2  <- (temp_dim_i[i2] - mosq_dim[k]):(temp_dim_i[i2] + mosq_dim[k])
temp_dim_j2  <- (temp_dim_j[j2] - mosq_dim[k]):(temp_dim_j[j2] + mosq_dim[k])
which_off_i2 <- which((temp_dim_i2 < 1) | (temp_dim_i2 > dim(landscape[[landscape_counter]])[1]))
which_off_j2 <- which((temp_dim_j2 < 1) | (temp_dim_j2 > dim(landscape[[landscape_counter]])[2]))
which_i2     <- seq(1, length(temp_dim_i2))
which_j2     <- seq(1, length(temp_dim_j2))

if (length(which_off_i2) > 0) {
  temp_dim_i2  <- temp_dim_i2[-which_off_i2]
  which_i2     <- which_i2[-which_off_i2]
}
if (length(which_off_j2) > 0) {
  temp_dim_j2  <- temp_dim_j2[-which_off_j2]
  which_j2     <- which_j2[-which_off_j2]
}
      
## What we now have is the distribution of mosquitoes with home range temp_dim_i[i2], temp_dim_j[j2], which we can use to 
 ## figure out what proportion of time those mosquitoes spend in i3, j3, where we are currently calculating infection as originating from
  ## Take the center home of mosquito at temp_dim_i[i2], temp_dim_j[j2], but subset its range by which_i2, which_j2 because of edge-effect problems

## Weight mosquito distribution by habitat preference. Assume the parameter listed earlier is a slope
if (mosq_hab_pref) {
  
## Set up what habitat the given mosquito is going to be weighted by. Non-dynamic and would need updating if new mosquitoes enter the fray
 ## Also, it is not at all obvious how to scale mosquito flight, so for now it may be best to just set mosq_hab_pref to be FALSE
  if (length(grep("aegypti", mosq_names[k])) == 1) {
     hab_mosq_weighted <- grep("urban", dimnames(landscape[[landscape_counter]])[[3]])
  } else if (length(grep("albopictus", mosq_names[k])) == 1) {
     hab_mosq_weighted <- grep("forest", dimnames(landscape[[landscape_counter]])[[3]])
  } else if (length(grep("Anopheles", mosq_names[k])) == 1) {
     hab_mosq_weighted <- grep("forest", dimnames(landscape[[landscape_counter]])[[3]])
  } else if (length(grep("Haemagogus", mosq_names[k])) == 1) {
     hab_mosq_weighted <- grep("forest", dimnames(landscape[[landscape_counter]])[[3]])
  } else {
    print("Error: Not a supported mosquito species"); break
  }
  
## Similar to the temp_weight calc for hosts from before
temp_weight <-   {
  if (boxed_in) {
((weighting_matrix[[k]][which_i2, which_j2] * landscape[[landscape_counter]][temp_dim_i2, temp_dim_j2, hab_mosq_weighted]) / 
  sum((weighting_matrix[[k]][which_i2, which_j2] * landscape[[landscape_counter]][temp_dim_i2, temp_dim_j2, hab_mosq_weighted])))
  } else {
  ((weighting_matrix[[k]][which_i2, which_j2] * landscape[[landscape_counter]][temp_dim_i2, temp_dim_j2, hab_mosq_weighted]) / 
  sum((weighting_matrix[[k]][which_i2, which_j2] * landscape[[landscape_counter]][temp_dim_i2, temp_dim_j2, hab_mosq_weighted]))) *
      sum(weighting_matrix[[k]][which_i2, which_j2])
  }
  }

temp_mosq_dist <- (temp_weight * landscape[[landscape_counter]][temp_dim_i[i2], temp_dim_j[j2], (num_land + num_hosts + k)])

} else {
  
temp_weight <- {
  if (boxed_in) {
  weighting_matrix[[k]][which_i2, which_j2] / sum(weighting_matrix[[k]][which_i2, which_j2])
  } else {
  weighting_matrix[[k]][which_i2, which_j2] / sum(weighting_matrix[[k]][which_i2, which_j2]) * sum(weighting_matrix[[k]][which_i2, which_j2])
  }
  }
  
temp_mosq_dist <- temp_weight * landscape[[landscape_counter]][temp_dim_i[i2], temp_dim_j[j2], (num_land + num_hosts + k)]
  
}

## Store all flight distributions of all of the mosquitoes that fly into i3, j3 (which are all mosquitoes
 ## in the loop over temp_dim_i2, temp_dim_j2)
mosq_temp_mosq_layers.weighting[[k]][[p]] <- temp_weight

## Store the flight
mosq_temp_mosq_layers[[k]][
  temp_dim_i2
, temp_dim_j2
, p] <- temp_mosq_dist

## Create a matrix that will be used later to identify all of the actual indices on the landscape of flight
mosq_temp_mosq_layers.coord.all[[k]][
  temp_dim_i2
, temp_dim_j2
, p
] <- 1

## and the home pixel as well
mosq_temp_mosq_layers.coord.home[[k]][
, 
, p] <- c(temp_dim_i[i2], temp_dim_j[j2])

## and expand that into a data frame for analysis later 
 ## To be explicit, this is a list of data frames that gives x, y coordinates that are "home bases" for all mosquitoes that spend some amount of
  ## time in cell i3, j3 which is where the current infected host now happens to find itself
mosq_temp_mosq_layers.coord.range[[k]][[p]] <- expand.grid(which_i2, which_j2)

p = p + 1

      }
  }

}

