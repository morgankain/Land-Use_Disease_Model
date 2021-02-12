########################################################################################################
### Run the WAIFW model across the landscape using the kernel for flight distance for all mosquitoes ### 
### and a similar kernel with different parameters for infected movement                             ###
########################################################################################################

## Code to run only a tiny fraction of the landscape for debugging purposes
if (extra_reduce) {
  landscape[[landscape_counter]] <- landscape[[landscape_counter]][23:26, 23:26, ] 
}

## Many empty result containers
source("results_containers.R")

## Counter for runs over the double loop over the landscape pixels
flow.t   <- 1
cum_time <- 0

## Loop over the rows and columns of the landscape matrix. Each [i, j] pair gives the source of infection for which we calculate R0
## (for the main text Methods conceptual figure used i = 21, j = 39)
for (i in 1:dim(landscape[[landscape_counter]])[1]) {
  for (j in 1:dim(landscape[[landscape_counter]])[2]) {
    
## intermediate products for host to mosquito and mosquito to host transmission
h_m_mat <- array(data = 0, dim = c(dim(landscape[[landscape_counter]])[c(1, 2)], num_mosq))
m_h_mat <- array(data = 0, dim = c(dim(landscape[[landscape_counter]])[c(1, 2)], num_hosts, num_mosq))
      
## Loop over the movement of HOSTS from i, j into the surrounding area -- the dispersal area of the infected HOST from i, j is defined by i3, j3
 ## Determine all of the MOSQUITOES that the infected HOST will interact within i3, j3, which is given by MOSQUTIOES that have at least one
  ## cell of their flight from their home base that causes them to be in i3, j3 for any amount of time. Thus these mosquitoes have wider range than i3, j3. 
    ## Also capture the flight of these (now infected) MOSQUITOES as they fly in their home range biting susceptible HOSTS

####
## Set up the pixels in which the infected hosts moves about
####
source("host_movement_setup.R")
   
## counter for cells the host moves into 
h_m.counter <- 1
 
## calculation over all of the cells the infected host moves into 
for (i3 in seq_along(temp_dim_i.h)) {
  for (j3 in seq_along(temp_dim_j.h)) {
  
temp_time   <- system.time({
   
#### 
## For the host within pixel i3, j3 determine the mosquito community in that cell given the surrounding mosquito community/landscape
####
source("parameters_dynamic.R")

#### 
## Now that the number of mosquitoes getting infected in i3, j3 has been calculated, take those mosquitoes
## and their flight distance and calculate how many (and where) hosts in the next generation are getting infected
#### 
source("R0_calc.R")

})

cum_time    <- cum_time + temp_time
h_m.counter <- h_m.counter + 1
# print(h_m.counter)
print(paste("i3 = ", i3, ";", "j3 = ", j3 ))
  }
}

## Script and counter is run whether host movement is considered or not
source("summarize_results.R")    

flow.t <- flow.t + 1
      
## If !host movement sim runs much faster so don't need to print results as often
if (!host_movement) {
 if (((flow.t / 5) %% 1) == 0) { print(paste(paste("i =", i, ";"), paste("j =", j), sep = " ")) }
} else {
 if (((flow.t / 5) %% 1) == 0) { print(paste(paste("i =", i, ";"), paste("j =", j), sep = " ")) }
}

if (((flow.t / 100) %% 1) == 0) {
  
if (sim_landscape) {
temp_nam <- paste(
 paste(
  paste("saved_output/landscape.out", Sys.Date(), sep = "_")
, paste(
  sim_landscape
, dens_run
, disease[disease.run]
, this_landscape_choice
, travel_dist[1]
, pop_exp
, sep = "_")
, sep = "_"
)
, ".Rds", sep = ""
)
} else {
temp_nam <- paste(
 paste(
  paste("saved_output/landscape.out", Sys.Date(), sep = "_")
, paste(
  sim_landscape
, this_landscape_choice
, disease[disease.run]
, host.run
, travel_dist[1]
, pop_exp
, sep = "_")
, sep = "_"
)
, ".Rds", sep = ""
)
}
  
try(saveRDS(landscape.out.top, temp_nam), silent = TRUE)
  
}

  }
  
}
