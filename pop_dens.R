#################################################################
### Obtain mosquito and other-host densities on the landscape ###
#################################################################

## Note: this script will be the one that changes substantially if the fundamentals of the model change
 ## (e.g. mosquito population is determined by new/different predictors)
  ## Script is somewhat dynamic, but ifelse obviously not dynamic so manual updates would be needed to multiple sections for model changes 

## First scale humans
 ## Could imagine some other method to scale here to form cities of some type, maybe an exp(1/X) of some kind.
if (sim_landscape) {
landscape[[landscape_counter]][,,"human"] <- landscape[[landscape_counter]][,,"urban"] * human_scaling
} ## NOTE: if !sim_landscape human is taken directly from the landscape

## Second get abundance of "other" species
 ## Needs to be adjusted if there is going to be more than one "other" species
if (disease[disease_counter] == "yellow_fever") {
landscape[[landscape_counter]][,,"primate"] <- alt_dens(trees = landscape[[landscape_counter]][,,"forest"], scaling = other_scaling*(primate_ratio)) 
landscape[[landscape_counter]][,,"other"]   <- alt_dens(trees = landscape[[landscape_counter]][,,"forest"], scaling = other_scaling*(1-primate_ratio))
} else {
landscape[[landscape_counter]][,,"other"]   <- alt_dens(trees = landscape[[landscape_counter]][,,"forest"], scaling = other_scaling)  
}

## Third get "home base" abundance of mosquitoes. Loop over each mosquito
 ## Series of assumptions here to establish how mosquito populations respond to the landscape and the populations
  ## of hosts that they bite
for (i in (num_land + num_hosts + 1):(num_land + num_hosts + num_mosq)) {
  
  if (dimnames(landscape[[landscape_counter]])[[3]][i] == "Aedes aegypti") {
    
temp_landscape <- landscape[[landscape_counter]][,,"urban"]
## Cant use 0 and 1 because of logit (cant have -inf and inf)
temp_landscape <- ifelse(temp_landscape == 1, 0.99, temp_landscape)
temp_landscape <- ifelse(temp_landscape == 0, 0.01, temp_landscape)

## Assume response to forest cover or urbanization on a logistic scale, where 1 returns the maximum possible mosquito to host ratio
  landscape[[landscape_counter]][,,i] <- mosq_dens(
    land              = qlogis(temp_landscape)
  , pop               =
       landscape[[landscape_counter]][,,"human"]  
    ### At one point I was scaling mosquito abundance by their biting preferences, but ended up moving away from this scaling,
      ### though am leaving this here because it could be an alternative sensible strategy 
    #  mosq_bite_pref["human", "Aedes aegypti"] + 
    #  landscape[[landscape_counter]][,,"other"] *
    #  mosq_bite_pref["other", "Aedes aegypti"] +
    #  landscape[[landscape_counter]][,,"primate"] *
    #  mosq_bite_pref["primate", "Aedes aegypti"]
  , ratio             = mosq_host_ratio["Aedes aegypti"]
  , interc            = mosq_hab_params[["Aedes aegypti"]]["interc"]
  , linear_scaling    = mosq_hab_params[["Aedes aegypti"]]["linear_scaling"]
  , quadratic_scaling = mosq_hab_params[["Aedes aegypti"]]["quadratic_scaling"]
  , pop_exp           = mosq_hab_params[["Aedes aegypti"]]["pop_exp"]
  , oth_scaling       = mosq_hab_params[["Aedes aegypti"]]["oth_scaling"]
  , oth_predictor     = mosq_hab_params[["Aedes aegypti"]]["oth_predictor"]
    )
  
  } else if (dimnames(landscape[[landscape_counter]])[[3]][i] == "Aedes albopictus") {
    
temp_landscape <- landscape[[landscape_counter]][,,"forest"]
temp_landscape <- ifelse(temp_landscape == 1, 0.99, temp_landscape)
temp_landscape <- ifelse(temp_landscape == 0, 0.01, temp_landscape)
    
## Assumption: some arbitrary parameters getting thrown around here, but somewhat unavoidable. Could consider some kind of sensitivity analysis
  landscape[[landscape_counter]][,,i] <- mosq_dens(
    land              = qlogis(temp_landscape)
  , pop               = 
  #    landscape[[landscape_counter]][,,"human"] * 
  #    mosq_bite_pref["human", "Aedes albopictus"] + 
       landscape[[landscape_counter]][,,"other"] + 
  #    mosq_bite_pref["other", "Aedes albopictus"]
      { 
      if (disease[disease_counter] == "yellow_fever") {
        landscape[[landscape_counter]][,,"primate"]
      } else {
       0
      }
      }
  #    mosq_bite_pref["primate", "Aedes albopictus"] 
  , ratio             = mosq_host_ratio["Aedes albopictus"]
  , interc            = mosq_hab_params[["Aedes albopictus"]]["interc"]
  , linear_scaling    = mosq_hab_params[["Aedes albopictus"]]["linear_scaling"]
  , quadratic_scaling = mosq_hab_params[["Aedes albopictus"]]["quadratic_scaling"]
  , pop_exp           = mosq_hab_params[["Aedes albopictus"]]["pop_exp"]
  , oth_scaling       = mosq_hab_params[["Aedes albopictus"]]["oth_scaling"]
  , oth_predictor     = mosq_hab_params[["Aedes albopictus"]]["oth_predictor"]
    )

  } else if (dimnames(landscape[[landscape_counter]])[[3]][i] == "Haemagogus sp") {
    
temp_landscape <- landscape[[landscape_counter]][,,"forest"]
temp_landscape <- ifelse(temp_landscape == 1, 0.99, temp_landscape)
temp_landscape <- ifelse(temp_landscape == 0, 0.01, temp_landscape)
    
## Assumption: some arbitrary parameters getting thrown around here, but somewhat unavoidable. Could consider some kind of sensitivity analysis
  landscape[[landscape_counter]][,,i] <- mosq_dens(
    land              = qlogis(temp_landscape)
  , pop               = 
    #   landscape[[landscape_counter]][,,"human"] * 
    #   mosq_bite_pref["human", "Haemagogus sp"] + 
        landscape[[landscape_counter]][,,"other"] + 
    #   mosq_bite_pref["other", "Haemagogus sp"] + 
        landscape[[landscape_counter]][,,"primate"]  
    #   mosq_bite_pref["primate", "Haemagogus sp"]  
  , ratio             = mosq_host_ratio["Haemagogus sp"]
  , interc            = mosq_hab_params[["Haemagogus sp"]]["interc"]
  , linear_scaling    = mosq_hab_params[["Haemagogus sp"]]["linear_scaling"]
  , quadratic_scaling = mosq_hab_params[["Haemagogus sp"]]["quadratic_scaling"]
  , pop_exp           = mosq_hab_params[["Haemagogus sp"]]["pop_exp"]
  , oth_scaling       = mosq_hab_params[["Haemagogus sp"]]["oth_scaling"]
  , oth_predictor     = mosq_hab_params[["Haemagogus sp"]]["oth_predictor"]
    )

  } else if (dimnames(landscape[[landscape_counter]])[[3]][i] == "Anopheles sp") {
  
temp_anoph_landscape <- array(
  dim = c(
    nrow(landscape[[landscape_counter]][,,"forest"])
  , ncol(landscape[[landscape_counter]][,,"forest"])
  , num_mosq
  , 2
  )
, data = 0)

dimnames(temp_anoph_landscape)[[4]] <- c("forest_mean", "forest_var")
  
## Anopheles prefer forest edge, which is a bit hard to quantify because I am not using categorical land cover.
 ## Instead, for each pixel calculate both the average forest cover and the variance in forest cover weighted by an 
  ## Anopheles in that pixel and the pixels within the flight distance of anopheles (a bit unclear as to the desired range here)
   ## Not an established method but produces reasonable, realistic results
    
## Because of map edge have to go through the steps seen elsewhere in this code to ignore absent pixels
 ## Loop over each mosquito and the full landscape matrix, cell by cell
for (l in 1:num_mosq) {
  for (j in 1:nrow(landscape[[landscape_counter]][,,"forest"])) {
    for (k in 1:ncol(landscape[[landscape_counter]][,,"forest"])) {
    
## checking where the mosquito flies based on its flight distance and recording which of these "falls off" the matrix
 ## this flight distribution is re-directed into the edge pixels (which means the edge has a higher density of mosquitoes
  ## than it would if it wasn't an edge)
temp_dim_j  <- (j - mosq_dim[l]):(j + mosq_dim[l])
temp_dim_k  <- (k - mosq_dim[l]):(k + mosq_dim[l])
which_off_j <- which(temp_dim_j < 1 | (temp_dim_j > nrow(landscape[[landscape_counter]][,,"forest"])))
which_off_k <- which(temp_dim_k < 1 | (temp_dim_k > ncol(landscape[[landscape_counter]][,,"forest"])))
which_j     <- seq(1, length(temp_dim_j))
which_k     <- seq(1, length(temp_dim_k))
if (length(which_off_j) > 0) {
temp_dim_j  <- temp_dim_j[-which_off_j]
which_j     <- which_j[-which_off_j]
}
if (length(which_off_k) > 0) {
temp_dim_k  <- temp_dim_k[-which_off_k]
which_k     <- which_k[-which_off_k]
}  

## Scale this by where the mosquito spends its time
temp_weight_mat <- weighting_matrix[[1]][which_j, which_k] / sum(weighting_matrix[[1]][which_j, which_k])

## Method for establishing the anopheles population as a ratio of both hosts but also the variance in 
 ## the forest, not obvious if using flight distance for forest but not pop is the way to go, but given no 
  ## established method that I could find, this gives reasonable and intuitive results
temp_anoph_range     <- landscape[[landscape_counter]][temp_dim_j, temp_dim_k, "forest"]
temp_anoph_range.wm  <- weighted.mean(temp_anoph_range, temp_weight_mat)
## Rather ugly fix for qlogis... Simply pull away from the boundaries
if (temp_anoph_range.wm == 1) {temp_anoph_range.wm <- 0.99} 
if (temp_anoph_range.wm == 0) {temp_anoph_range.wm <- 1.00}

temp_anoph_landscape[j, k, l, "forest_mean"] <- qlogis(temp_anoph_range.wm)
  ## Weighted variance calculation
temp_anoph_landscape[j, k, l, "forest_var"]  <- sum(temp_weight_mat * (temp_anoph_range - temp_anoph_range.wm)^2)

    }
  }
}

## Again, few too many arbitrary scaling parameters floating around here, but given my desire to keep the same function
 ## for all mosquitoes, a necessity
landscape[[landscape_counter]][,,i] <- mosq_dens(
    land              = temp_anoph_landscape[,, l, "forest_mean"]
  , pop               = 
    landscape[[landscape_counter]][,,"human"] * 
    mosq_bite_pref["human", "Anopheles sp"] + 
    landscape[[landscape_counter]][,,"other"] *
    mosq_bite_pref["other", "Anopheles sp"]
  , ratio             = mosq_host_ratio["Anopheles sp"]
  , interc            = mosq_hab_params[["Anopheles sp"]]["interc"]
  , linear_scaling    = mosq_hab_params[["Anopheles sp"]]["linear_scaling"]
  , quadratic_scaling = mosq_hab_params[["Anopheles sp"]]["quadratic_scaling"]
  , pop_exp           = mosq_hab_params[["Anopheles sp"]]["pop_exp"]
  , oth_scaling       = mosq_hab_params[["Anopheles sp"]]["oth_predictor"] / anoph_scale_1
  , oth_predictor     = log(temp_anoph_landscape[,, l, "forest_var"] * anoph_scale_2)
    )
    
  }
}

########
### Just for visualization purposes of mosquito flight on the landscape, not actually used for anything 
########

## Third, get the number of mosquitoes flying around in cell i, j, which is
 ## going to be the sum of all mosquitoes that are flying through that cell
mosq_temp_dens <- array(data = 0, dim = c(
  dim(landscape[[landscape_counter]])[1]
, dim(landscape[[landscape_counter]])[2]
, length((num_land + num_hosts):(num_land + num_hosts + num_mosq - 1))
  ))

## For every cell i, j need to apply the weighting matrix filter, then deal
 ## with edge effects by dropping mosquito distribution that resides off of the 
  ## plotted landscape[[landscape_counter]]
for (k in 1:dim(mosq_temp_dens)[3]) {      ## mosquitoes
  for (j in 1:dim(mosq_temp_dens)[2]) {    ## columns
    for (i in 1:dim(mosq_temp_dens)[1]) {  ## rows
      
temp_dim_i  <- (i - mosq_dim[k]):(i + mosq_dim[k])
temp_dim_j  <- (j - mosq_dim[k]):(j + mosq_dim[k])
which_off_i <- which((temp_dim_i < 1) | (temp_dim_i > dim(mosq_temp_dens)[1]))
which_off_j <- which((temp_dim_j < 1) | (temp_dim_j > dim(mosq_temp_dens)[2]))
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

## number of mosquitoes from cell i, j flying about      
 ## add these, looping over i, j, to the temp matrix and then sub into the landscape[[landscape_counter]] matrix
if (mosq_hab_pref) {
  
  temp_weight <- ((weighting_matrix[[k]][which_i, which_j] * landscape[[landscape_counter]][temp_dim_i, temp_dim_j, k]) / 
  sum((weighting_matrix[[k]][which_i, which_j] * landscape[[landscape_counter]][temp_dim_i, temp_dim_j, k]))) *   {
  if (boxed_in) {
  1
  } else {
  sum(weighting_matrix[[k]][which_i, which_j])
  }
  }
  
temp_mosq_dist <- (temp_weight * landscape[[landscape_counter]][temp_dim_i, temp_dim_j, (num_land + num_hosts - 1 + k)])

mosq_temp_dens[temp_dim_i, temp_dim_j, k] <- mosq_temp_dens[temp_dim_i, temp_dim_j, k] + temp_mosq_dist
  
} else {
  
  temp_weight <- weighting_matrix[[k]][which_i, which_j] / sum(weighting_matrix[[k]][which_i, which_j]) *   {
  if (boxed_in) {
  1
  } else {
  sum(weighting_matrix[[k]][which_i, which_j])
  }
  }
  
temp_mosq_dist <- (temp_weight * landscape[[landscape_counter]][temp_dim_i, temp_dim_j, (num_land + num_hosts - 1 + k)])

mosq_temp_dens[temp_dim_i, temp_dim_j, k] <- mosq_temp_dens[temp_dim_i, temp_dim_j, k]  + temp_mosq_dist
  
}

    }
  }
}

## Finally, use the start_weight argument to build a map of scaling coefficients for where the starting infection is likely to appear

 if (start_scaling == "weighted") {
   ## not the most sensible, so leave this for later (and maybe remove it...)
   
 } else if (start_scaling == "single") {
     
   if (disease[disease_counter] == "dengue") {
      
      start.weight_mat <- landscape[[landscape_counter]][,,"human"] / sum(landscape[[landscape_counter]][,,"human"])
     
   } else if (disease[disease_counter] == "yellow_fever") {
     
      start.weight_mat <- landscape[[landscape_counter]][,,"primate"] / sum(landscape[[landscape_counter]][,,"primate"])
     
   } else {
     
      start.weight_mat <- landscape[[landscape_counter]][,,"Anopheles sp"] / sum(landscape[[landscape_counter]][,,"Anopheles sp"])
     
   }
   
   ## if flat, just multiply by one in all cells
 } else {
   
   start.weight_mat <- matrix(data = 1, nrow = dim(landscape[[landscape_counter]])[1], ncol = dim(landscape[[landscape_counter]])[2])
   
 }


