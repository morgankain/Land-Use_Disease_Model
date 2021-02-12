###############################################################################################
### For R0 calculation need to figure out where the infected host spends its time and       ###
### what the host communities look like in these map pixels (in order to determine the      ###
### biting of mosquitoes that find themselves in this pixel)                                ###
###############################################################################################

## First figure out where the infected host spends its time (within the given landscape[[landscape_counter]] matrix). 
 ## This first section determines where a host with infection origin in i, j moves about over the course of its infection
temp_dim_i.h  <- (i - host_dim[as.logical(which_adj[[disease_counter]])]):(i + host_dim[as.logical(which_adj[[disease_counter]])])
temp_dim_j.h  <- (j - host_dim[as.logical(which_adj[[disease_counter]])]):(j + host_dim[as.logical(which_adj[[disease_counter]])])
which_off_i.h <- which((temp_dim_i.h < 1) | (temp_dim_i.h > dim(landscape[[landscape_counter]])[1]))
which_off_j.h <- which((temp_dim_j.h < 1) | (temp_dim_j.h > dim(landscape[[landscape_counter]])[2]))
which_i.h     <- seq(1, length(temp_dim_i.h))
which_j.h     <- seq(1, length(temp_dim_j.h))
if (length(which_off_i.h) > 0) {
temp_dim_i.h  <- temp_dim_i.h[-which_off_i.h]
which_i.h     <- which_i.h[-which_off_i.h]
}
if (length(which_off_j.h) > 0) {
temp_dim_j.h  <- temp_dim_j.h[-which_off_j.h]
which_j.h     <- which_j.h[-which_off_j.h]
}
  
## Abundance of the infected host in these pixels
count_hosts_focal.h <- matrix(
  nrow = length(temp_dim_i.h)
, ncol = length(temp_dim_j.h)
, data = landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, starting_host[starting_host_counter]]
)

## Abundance of the other host types in these pixels
if (disease[disease_counter] != "yellow_fever") {
count_hosts_focal.o <- matrix(
  nrow = length(temp_dim_i.h)
, ncol = length(temp_dim_j.h)
, data = landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, "other"]
)
} else {
count_hosts_focal.o1 <- matrix(
  nrow = length(temp_dim_i.h)
, ncol = length(temp_dim_j.h)
, data = {
  if (starting_host[starting_host_counter] == "human") {
  landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, "primate"]
  } else {
  landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, "human"] 
  }
}
) 
count_hosts_focal.o2 <- matrix(
  nrow = length(temp_dim_i.h)
, ncol = length(temp_dim_j.h)
, data = landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, "other"]
)

}

## Last setup piece needed before calculating disease spread is to scale where the infected will spend its time given
 ## landscape characteristics (if desired)
if (host_hab_pref) {
  
## Scale by human population or forest depending on the host
host_layer <- ifelse(which_adj[[disease_counter]][1] == 1, 1, 3)
  
## Weight host movement by habitat preference. Assume the parameter listed earlier is a slope
 ## And yes, slightly weird thing going on here with preserving edge effect
temp_weight.h <- {
  if (boxed_in) {
    ((weighting_matrix.h[[which(which_adj[[disease_counter]] == 1)]][which_i.h, which_j.h] * 
    landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, host_layer]) / 
  sum((weighting_matrix.h[[which(which_adj[[disease_counter]] == 1)]][which_i.h, which_j.h] *
    landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, host_layer])))
  } else {
  ((weighting_matrix.h[[which(which_adj[[disease_counter]] == 1)]][which_i.h, which_j.h] * 
     landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, host_layer]) / 
  sum((weighting_matrix.h[[which(which_adj[[disease_counter]] == 1)]][which_i.h, which_j.h] *
    landscape[[landscape_counter]][temp_dim_i.h, temp_dim_j.h, host_layer]))) * 
  sum(weighting_matrix.h[[which(which_adj[[disease_counter]] == 1)]][which_i.h, which_j.h])
  }
}
  
} else {
  
if (boxed_in) {
  
temp_weight.h <- weighting_matrix.h[[which(which_adj[[disease_counter]] == 1)]][which_i.h, which_j.h]
  
} else {
  
temp_weight.h <- weighting_matrix.h[[which(which_adj[[disease_counter]] == 1)]][which_i.h, which_j.h] / 
    sum(weighting_matrix.h[[which(which_adj[[disease_counter]] == 1)]][which_i.h, which_j.h])
  
}
  
}

## For !host_movement have to convert from a scalar to a matrix for downstream code
if (class(temp_weight.h)[1] == "numeric") {
 temp_weight.h <- matrix(data = temp_weight.h, nrow = 1, ncol = 1) 
}

