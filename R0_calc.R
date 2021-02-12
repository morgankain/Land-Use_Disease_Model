#################################################################################
### Calculate host-to-mosquito transmission and mosquito-to-host transmission ###
#################################################################################

## Note: For a given loop iteration, the calculation here first determines how many mosquitoes will get infected in that cell (i3, j3) 
 ## and then calculates how many (and stores the cells) hosts get infected from those mosquitoes dispersing over their area 
  ## !!Note also on the semi-opaque way these indices and matrices are set up: 
   ## -- temp_dim_i.h[i3] and temp_dim_j.h[j3] is used when looking where _on the landscape_ the host is
    ## (which is needed for finding the mosquitoes _on the landscape_)
   ## -- while i3, j3 is used when looking _where in the hosts' distribution_ the host is

## Loop over all types of mosquitoes that are getting infected and then infecting new hosts
for (k in 1:num_mosq) {
  
## Physiological capability of the infected host in infecting mosquitoes
WAIFW_right_s1        <- colSums(h_to_m_trans[,,k]) / daily_scaling ## scale back 

## Container for the infected mosquitoes in each of the cells that distribute mosquitoes into i3, j3 where
 ## the infected host resides
WAIFW_right           <- array(data = 0, dim = c(1, 1, dim(mosq_temp_mosq_layers[[k]])[3]))

## Biting preference of mosquitoes biting in cell i3, j3 where the infection is
 ## Calculated as biting preference * abundance on focal / sum(biting pref * abundance) over all in the focal cell
mosq_bite_pref.f      <- mosq_bite_pref[starting_host[starting_host_counter], k] * (count_hosts_focal.h[i3, j3] + temp_weight.h[i3, j3])
if (disease[disease_counter] == "yellow_fever") {
mosq_bite_pref.o      <- sum(mosq_bite_pref[-which(dimnames(mosq_bite_pref)[[1]] == starting_host[starting_host_counter]), k] *
  c(count_hosts_focal.o1[i3, j3], count_hosts_focal.o2[i3, j3]))
} else {
mosq_bite_pref.o      <- mosq_bite_pref[-which(dimnames(mosq_bite_pref)[[1]] == starting_host[starting_host_counter]), k] * count_hosts_focal.o[i3, j3] 
}

mosq_bite_home <- mosq_bite_pref.f / (mosq_bite_pref.f + mosq_bite_pref.o)

## Fill in the infected mosquitoes with biting pref and host abundance
WAIFW_right[,,] <- 
    ## Probability that the infection is in this starting cell 
  start.weight_mat[i, j] *
    ## Time that the infection spends in pixel i3, j3
  temp_weight.h[i3, j3] *
    ## biting rate of each mosquito in units of /day
  daily_bites[k]        * 
    ## proportion of bites on the host type which contains the infected
  mosq_bite_home *
    ## ratio of the number of mosquitoes per host type of the host type that contains the infection
     ## (this specifically calculates the proportion of time each mosquito with a home range around [i3, j3] spends in [i3, j3])
  (mosq_temp_mosq_layers[[k]][temp_dim_i.h[i3], temp_dim_j.h[j3], ] / 
      (sum(count_hosts_focal.h[i3, j3] + temp_weight.h[i3, j3]))) *
    ## physiological ability to infect mosquitoes (calculated above)
  WAIFW_right_s1[as.logical(which_adj[[disease_counter]])] 

## For any host == 0 in a focal cell need a check to convert from NA to 0
WAIFW_right <- apply(WAIFW_right, 1:3, FUN = function(x) ifelse(is.nan(x), 0, x))

## Extract the home of each of the mosquitoes that distribute mosquitoes into i, j
mosq_home       <- mosq_temp_mosq_layers.coord.home[[k]]

####
## Now that we have the infected mosquitoes, calculate the dispersal of infection into the next generation of currently susceptible hosts
####

## Determine the neighborhoods of each of the infected mosquitoes for sending infection back into the host community for the next generation
 ## So annoying that this sometimes returns a list and sometimes returns a matrix 
  ## (something pretty internal with the properties of the array being sent in)
   ## This is clunkier, but does force it to be a nested list, then removes the internal list layers (adds a trivial amount of time only)
# mosq_coverage   <- apply(mosq_temp_mosq_layers[[k]], 3, FUN = function (x) list(which(x != 0)))
mosq_coverage   <- apply(mosq_temp_mosq_layers.coord.all[[k]], 3, FUN = function (x) list(which(x == 1)))

## To repeat in different language: this gives a list of all of the cell id into which each mosquito that flies into i3, j3 will disperse infection into
mosq_coverage   <- lapply(mosq_coverage, "[[", 1)

## Calculate the abundance of each host in all of the cells that the infected mosquitoes visit and scale
 ## this abundance by biting preference
if (disease[disease_counter] != "yellow_fever") {
temp_humans     <- lapply(mosq_coverage , FUN = function (x) landscape[[landscape_counter]][,,"human"][x])
temp_other      <- lapply(mosq_coverage , FUN = function (x) landscape[[landscape_counter]][,,"other"][x])  
temp_counts     <- Map(rbind, temp_humans, temp_other)
} else {
temp_humans     <- lapply(mosq_coverage , FUN = function (x) landscape[[landscape_counter]][,,"human"][x])
temp_other1     <- lapply(mosq_coverage , FUN = function (x) landscape[[landscape_counter]][,,"primate"][x])
temp_other2     <- lapply(mosq_coverage , FUN = function (x) landscape[[landscape_counter]][,,"other"][x])
temp_counts     <- Map(rbind, temp_humans, temp_other1, temp_other2)
}

temp_prop       <- lapply(temp_counts   , FUN = function (x) sweep(x, 2, colSums(x), "/"))
mosq_bite_away  <- lapply(temp_prop     , FUN = function (x) mosq_bite_pref[, k] * x)
mosq_bite_away  <- lapply(mosq_bite_away, FUN = function (x) sweep(x, 2, colSums(x), "/"))

## Mosquito transmission of an infected mosquito back to its neighborhood
temp_dist       <- lapply(mosq_temp_mosq_layers.weighting[[k]], FUN = function (x)
  t(replicate(num_hosts, c(x))) * sum(m_to_h_trans_s[,k]) * daily_bites[k] 
  )

 ## Also slightly annoying that if the mosquito is only moving in one grid cell, then this gets returned as a matrix with one
  ## row which needs to get transposed 
if (nrow(temp_dist[[1]]) == 1) {
  temp_dist[[1]] <- t(temp_dist[[1]])
}

WAIFW_left      <- try(Map("*", mosq_bite_away, temp_dist), silent = TRUE)
if (class(WAIFW_left) == "try-error") {
print(paste(
  "i = ", i, ";", "j = ", j, "::", "i3 =", i3, ";", "j3 = ", j3, "::"
, "mosq_bite_away:", "length = ", length(mosq_bite_away)
, "dim = ", dim(mosq_bite_away[[1]])[1], "x", dim(mosq_bite_away[[1]])[2]
, "temp_dist:", "length = ", length(temp_dist)
, "dim = ", dim(temp_dist[[1]])[1], "x", dim(temp_dist[[1]])[2], sep = " ")) 
}

temp_mmat       <- try(Map("*", WAIFW_right, WAIFW_left), silent = TRUE)
if (class(temp_mmat) == "try-error") {
print(paste(
  "i = ", i, ";", "j = ", j, "::", "i3 =", i3, ";", "j3 = ", j3, "::"
, "WAIFW_right:", "length = ", length(WAIFW_right)
, "dim = ", dim(WAIFW_right[[1]])[1], "x", dim(WAIFW_right[[1]])[2]
, "WAIFW_left:", "length = ", length(WAIFW_left)
, "dim = ", dim(WAIFW_left[[1]])[1], "x", dim(WAIFW_left[[1]])[2], sep = " "))
}

## So close to removing all loops -- but cant get this without a loop because of the sub-setting :(
 ## Not much of a big deal because this is pretty fast anyway
for (k2 in 1:dim(mosq_temp_mosq_layers[[k]])[3]) {

## Supplement with an addition. Counts the next piece of the distribution of infection propagation
  h_m_mat[mosq_home[,,k2][1], mosq_home[,,k2][2], k] <- h_m_mat[mosq_home[,,k2][1], mosq_home[,,k2][2], k] + WAIFW_right[,,k2]

## Find all of the cells into which the infected mosquitoes will infect new hosts
for (w in 1:num_hosts) {
  m_h_mat[,,w,k][mosq_coverage[[k2]]] <- m_h_mat[,,w,k][mosq_coverage[[k2]]] + temp_mmat[[k2]][w, ]
}

  }

}
