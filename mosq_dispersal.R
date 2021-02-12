#####################################################################
### Determine the expected number of host-seeking mosquitoes in a ###
### given landscape cell, given cells around that "home" cell and ###
### mosquito flight distance                                      ###
#####################################################################

## Assume there is some inherent resting density of mosquitoes in a given grid cell based on
 ## characteristics of that cell that lead to production of new adults in that cell. This cell
  ## is assumed to serve as adults "home base" in its flight patterns. 

## Also assume a mosquito flies around this "home base" with some flight pattern that is a decay function
 ## around that home base, which we will model as a Gaussian kernel with sd = some function of flight distance
  ## that will be discretized and scaled to the size of the maps (imported or simulated) grid cells.
   ## Allow mosquito flight (and host movement) to weight movement to its preferred habitat type (or not) (see parameters.R)

## Use spatialfil::convKernel to generate Gaussian spatial kernel (other options are available as well)
 ## Need to be careful about what sd to use depending on the scale of the matrix
  ## the function uses floor(sd * 7 / 2) to define the dimensions of the matrix that it spits out, so can
   ## use this to define the sd depending on the scale of the matrix

## Number of grid cells a mosquito will experience
grid.flight  <- ifelse(flight_dist / matrix_scale[[landscape_counter]] < 1, 1, flight_dist / matrix_scale[[landscape_counter]])
sigma.flight <- (grid.flight * 2) / 7

weighting_matrix <- vector("list", num_mosq)
mosq_dim         <- numeric(num_mosq)

for (i in seq_along(sigma.flight)) {

## First check if flight distance is less than or equal to the pixel dimensions
  if (flight_dist[i] <= matrix_scale[[1]]) {
   ## Yes, this could get a little funky if the mosquitoes flight distance is much less than the dimensions of each pixel. Avoid this
weighting_matrix[[i]] <- matrix(nrow = 1, ncol = 1, data = 1)
  } else {
## proportion of time a mosquito resides in any grid cell around its home base (gets adjusted by habitat preference later)
weighting_matrix[[i]] <- convKernel(sigma = sigma.flight[i], k = c("gaussian"))$matrix
  if (sum(weighting_matrix[[i]]) > 1) {
weighting_matrix[[i]] <- weighting_matrix[[i]] / sum(weighting_matrix[[i]])  
  }
  }

## Flight radius in terms of grid cells
mosq_dim[i]           <- (dim(weighting_matrix[[i]]) - 1)[1] / 2
  
}

## Host movement. Reduces to no movement (hosts spend all of their time in one pixel) if !host_movement
 ## Assumption: hosts move relatively little when they are sick. Note: we only care about "movement" that allows hosts
  ## to interact with mosquitoes, so going from home to a hospital and back home will have negligible impact
   ## Could also consider host movement as an inverse proportion to their titer. By no means perfect in terms of 
    ## timing, but semi-realistic. However, would take another loop over time so too slow computationally 

grid.travel  <- travel_dist / matrix_scale[[landscape_counter]]
sigma.travel <- (grid.travel * 2) / 7  

weighting_matrix.h <- vector("list", num_hosts)
host_dim           <- numeric(num_hosts)

for (i in seq_along(sigma.travel)) {
  
if (travel_dist[i] <= matrix_scale[[1]]) {
  weighting_matrix.h[[i]] <- matrix(nrow = 1, ncol = 1, data = 1)
} else {
  
  weighting_matrix.h[[i]] <- convKernel(sigma = sigma.travel[i], k = c("gaussian"))$matrix
  
  if (sum(weighting_matrix.h[[i]]) > 1) {
  weighting_matrix.h[[i]] <- weighting_matrix.h[[i]] / sum(weighting_matrix.h[[i]])  
  }
  
}

  host_dim[i]             <- (dim(weighting_matrix.h[[i]]) - 1)[1] / 2
}

