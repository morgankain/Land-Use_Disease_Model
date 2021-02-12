#####################################################################################
### Establish parameters for transmission for the hosts and mosquitoes            ###
### Placing this script inside of the loop over diseases allows for the parameter ###
### values for the current loop to be extracted as necessary                      ###
#####################################################################################

## NOTE:
  ## 1) Because of sparse quantitative data a number of these parameters are a translation of qualitative descriptions (e.g., sometimes as little as
   ## ) "common knowledge" about the disease systems propagated in the lit) to quantitative values, nonetheless...
  ## 2) ... parameter values are backed up by as much lit as possible

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
## -----------------------------------------------------------------------------------------------------------------------------------------------
## ----------------------------------
## -- Parameter values and details --
## ----------------------------------
## 
## Daily Survival:
## ---------
## Anopheles        -- average of all of the estimates:  0.82
## Aedes aegypti    -- average of the few estimates:     0.85
## Aedes albopictus -- average of the few estimates:     0.91
## Haemegogous      -- average of all of the estimates:  0.93
## 
## Biting preference (human:other):
## ------------------
## Anopheles        -- Median of all of the estimates:  0.361:1
## Aedes aegypti    -- Median of the few estimates:     5.37:1
## Aedes albopictus -- Median of the few estimates:     5.41:1
## Haemegogous      -- Median of the few estimates:     0.202:1
## 
## Habitat preference:
## -------------------
## Anopheles        -- Preference for edge habitat. This knowledge is converted into a quantitative measure of where mosquitoes are found 
## Aedes aegypti    -- Increases with human population
## Aedes albopictus -- Somewhat unclear, but clearly are less urban specialists than aegypti and often can be found breeding in natural structures like ponds and tree holes 
## Haemagogus       -- Somewhat unclear, generally viewed to be a forest specialist, but more recent work (Alencar et al. 2018) suggests that less forest cover (and thus more fragmentation
##                       may promote higher Haemagogus abundance), and other work has readily found Haemagogus species at forest edges (Hendy et al. 2020). Here, we model
##                       Haemagogus as dependent only on the pixel of forest cover at hand, but allow for dispersal into less forested area to capture this dual dependence
## 
## Biting rate:
## ------------
##     Using Mordecai et al. 2013 and the average of the briere-derived temperature-dependent biting rate over 20 to 40 C returns:
##       0.37 for aedes aegypti and 0.55 for anopholes pseudopunctipennis. However, other evidence suggests Aedes aegypti bites multiple times
##        per gonotrophic cycle, and if this this is the case and a cycle length is something like three days, this biting rate needs to be higher
## Anopheles        -- 0.37
## Aedes aegypti    -- 0.37 * 1.5 = 0.555
## Aedes albopictus -- 0.55
## Haemegogous      -- 0.122 calculated as 1 / gonotrophic cycle length estimated from wild caught mosquitoes 
##
## Flight distance:
## ----------------
##     Direct estimates from a few papers that find major nesting sites and evidence where those mosquitoes are biting
##       Loosest by far is Anopheles because of aggregation to species 
## Aedes aegypti    -- 200m
## Aedes albopictus -- 600m
## Anopheles sp     -- 600m
## Haemegogous sp   -- 600m (in the dispersal model of Childs et al. 2016, based on an exponential decay model fit to data taken from the lit, under 1% of all mosquitoes were found to venture further than 600m)
## 
## Mosquito-to-host ratio:
## -----------------------
##     Definitely the  most difficult parameter (but needed to get absolute values of R0 --- not relevant for relative values). Assuming same raw abundance of the 3 mosquitoes to total hosts, but  
##     assume that their distribution varies so pixel by pixel the ratio of mosquitoes to humans and others will vary. 
## Anopheles        -- 20:1 (bumping this a tad to correct for an overly small R0 value. This is unlikely to be the place -in reality- for why R0 is estimated to be too
##                           low, but because of the lack of data and the fact that this is just a scalar, it is the easiest method). Also, because the other diseases
##                           get multiple mosquitoes transmitting them, this ratio is still smaller than the total mosquito:human ratio for the other diseases
## Aedes aegypti    -- 5:1
## Aedes albopictus -- 5:1
## Haemegogous sp   -- 5:1 
## 
## Host abundance
## --------------
## Human   -- Dependent on urban areas based on the linear scalar human_scaling
## Other   -- Dependent on forest based on the linear scalar other_scaling
## Primate -- Only relevant for yellow fever model. No data, so assume Primates are some portion of all "others" (see primate_ratio below)
##
## Host pathogen load:
## -------------------
## Humans with Dengue        -- Model fit to data
## Humans with Yellow Fever  -- Assumption of lower than other 
## Humans with Malaria       -- Model fit to data (extended with a few assumptions)
## Others with Dengue        -- Assumption of lower than humans
## Others with Yellow Fever  -- Model fit to data (two species)
## Others with Malaria       -- Assumption of much lower than humans
## 
## Host-to-mosquito transmission:
## ------------------------------
## Aedes aegypti Dengue           -- Model fit to data
## Aedes aegypti Yellow Fever     -- One data set, pulled along with model fit to aedes albopictus
## Aedes aegypti Malaria          -- Cant transmit
## Aedes albopictus Dengue        -- One citation, pulled along with the mdoel fit to aedes aegypti
## Aedes albopictus Yellow Fever  -- Model fit to data
## Aedes albopictus Malaria       -- Cant transmit
## Anopheles sp Dengue            -- Cant transmit
## Anopheles sp Yellow Fever      -- Cant transmit
## Anopheles sp Malaria           -- Model fit to data
## Haemagogus sp Dengue           -- Not a transmitter 
## Haemagogus sp Malaria          -- Not a transmitter 
## Haemagogus sp Yellow Fever     -- Assumed same response as Aedes 
##
## Mosquito-to-host transmission:
## ------------------------------
## Aedes aegypti Dengue           -- Model fit to data
## Aedes aegypti Yellow Fever     -- One citation, pulled along with the mdoel fit to aedes albopictus
## Aedes aegypti Malaria          -- Cant transmit
## Aedes albopictus Dengue        -- One citation, pulled along with the mdoel fit to aedes aegypti
## Aedes albopictus Yellow Fever  -- Model fit to data
## Aedes albopictus Malaria       -- Cant transmit
## Anopheles sp Dengue            -- Cant transmit
## Anopheles sp Yellow Fever      -- Cant transmit
## Anopheles sp Malaria           -- Model fit to data
## Haemagogus sp Dengue           -- Not a transmitter 
## Haemagogus sp Malaria          -- Not a transmitter 
## Haemagogus sp Yellow Fever     -- Model fit to data (data sources gathered in Childs et al. 2019)
## 
## -----------------------------------------------------------------------------------------------------------------------------------------------
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

## Map parameters
 ## length and width of each grid cell in meters for the given map layers. 
  ## These need to be the same or things will break
matrix_scale    <- lapply(1:num_landscapes, function(x) 200)

## number of hosts; for now assume humans and "others" which is some average of all spillover hosts
 ## (later "other" is split to non-human primates and non-human, non-primates for yellow fever later)
num_hosts       <- 2     
host_names      <- c("human", "other")

## number of mosquitoes. Currently set up relatively non-dynamically to depend on specific diseases.
 ## Just one in a laundry list of other changes that would be needed to add another disease
if (disease[disease_counter] == "dengue") {

num_mosq        <- 2     

## Mosquito species 
mosq_spec       <- list(
  data.frame(genus   = "Aedes" 
  ,          species = "aegypti" 
  )
, data.frame(genus   = "Aedes"
  ,          speceis = "albopictus"
  ))

mosq_names <- c("Aedes aegypti", "Aedes albopictus")

## Current parameterization requires knowledge about maximum mosquito-host ratio which is basically impossible to come up with, but not
 ## sure of a way around this. Will end up focusing on relative R0 anyway instead of absolute, so the actual value here won't matter _much_
mosq_host_ratio   <- c("Aedes aegypti" = 5, "Aedes albopictus" = 5)

## How mosquitoes respond to urban area and tree cover. For details on these parameters see functions.R "mosq_dens" 
 ## and the lengthy description of this function in the supplemental methods of the manuscript 
  ## Note: it is worth plotting/exploring what this function looks like across paramater vlaues if you are a user interested in running the model (also see supplemental methods of the paper)
 ## I admit that this function is quite confusing: I wanted one function to model all mosquitoes in such a way that
  ## someone could _conceivably_ fit to data and plug in parameter values...
   ## These are used in pop_dens_kernel.R.
mosq_hab_params   <- list(
 "Aedes aegypti"      = c(
  "interc"            = 0
, "linear_scaling"    = 0.75
, "quadratic_scaling" = 0
, "pop_exp"           = pop_exp
, "oth_scaling"       = 0.5
, "oth_predictor"     = 1
  )
, "Aedes albopictus"  = c(
  "interc"            = 0 
, "linear_scaling"    = 0.75
, "quadratic_scaling" = 0
# , "pop_exp"           = pop_exp
, "pop_exp"           = 1
, "oth_scaling"       = 0.5
, "oth_predictor"     = 1
  )
  )

## Biting preference of each mosquito on each host. Currently, non-dynamic and would need to be updated to consider more diseases
mosq_bite_pref <- matrix(                 
  ncol = num_mosq
, nrow = num_hosts
, data =
    c(
  c(bite.data.ae.bite[bite.data.ae.bite$Species == "aegypti", ]$FR, 1 - bite.data.ae.bite[bite.data.ae.bite$Species == "aegypti", ]$FR)       
, c(bite.data.ae.bite[bite.data.ae.bite$Species == "albopictus", ]$FR, 1 - bite.data.ae.bite[bite.data.ae.bite$Species == "albopictus", ]$FR)     
    )
, byrow = FALSE
)

dimnames(mosq_bite_pref) <- 
  list(
    c("human", "other")
  , mosq_names)

} else if (disease[disease_counter] == "yellow_fever") {
  
num_mosq        <- 3 
num_hosts       <- 3 ## overwrite with three hosts only for yellow fever  
host_names      <- c("human", "primate", "other")

## Mosquito species 
mosq_spec       <- list(
  data.frame(genus   = "Aedes" 
  ,          species = "aegypti")
, data.frame(genus   = "Aedes"
  ,          speceis = "albopictus")
, data.frame(genus   = "Haemagogus" 
  ,          species = "sp")
  )

mosq_names <- c("Aedes aegypti", "Aedes albopictus", "Haemagogus sp")

## Current parameterization requires knowledge about maximum mosquito-host ratio which is basically impossible to come up with, but not
 ## sure of a way around this. Will end up focusing on relative R0 anyway instead of absolute, so the actual value here won't matter _much_
mosq_host_ratio   <- c("Aedes aegypti" = 5, "Aedes albopictus" = 5, "Haemagogus sp" = 5)

## How mosquitoes respond to urban area and tree cover --- see more extensive commenting in the dengue entry above
mosq_hab_params   <- list(
 "Aedes aegypti"      = c(
  "interc"            = 0
, "linear_scaling"    = 0.75
, "quadratic_scaling" = 0
, "pop_exp"           = pop_exp
, "oth_scaling"       = 0.5
, "oth_predictor"     = 1
  )
, "Aedes albopictus"  = c(
  "interc"            = 0 
, "linear_scaling"    = 0.75
, "quadratic_scaling" = 0
# , "pop_exp"           = pop_exp
, "pop_exp"           = 1
, "oth_scaling"       = 0.5
, "oth_predictor"     = 1
  )
, "Haemagogus sp"     = c(             
  "interc"            = 0 
, "linear_scaling"    = 0.75
, "quadratic_scaling" = 0
#, "pop_exp"           = pop_exp
, "pop_exp"           = 1
, "oth_scaling"       = 0.5
, "oth_predictor"     = 1
  )
  )


 ## The assumption here in the absence of better data is that mosquitoes will split the non-human bites between the host of interest (primate) and the useless sink "other" host
primate_ratio <- 0.5

mosq_bite_pref <- matrix(                 
  ncol = num_mosq
, nrow = num_hosts
, data =
    c(
  c(bite.data.ae.bite[bite.data.ae.bite$Species == "aegypti", ]$FR
    , (1 - bite.data.ae.bite[bite.data.ae.bite$Species == "aegypti", ]$FR)*(primate_ratio)
    , (1 - bite.data.ae.bite[bite.data.ae.bite$Species == "aegypti", ]$FR)*(1 - primate_ratio))       
, c(bite.data.ae.bite[bite.data.ae.bite$Species == "albopictus", ]$FR
    , (1 - bite.data.ae.bite[bite.data.ae.bite$Species == "albopictus", ]$FR)*(primate_ratio)
    , (1 - bite.data.ae.bite[bite.data.ae.bite$Species == "albopictus", ]$FR)*(1 - primate_ratio))    
, c(bite.data.ha.bite$FR, (1 - bite.data.ha.bite$FR)*(primate_ratio), (1 - bite.data.ha.bite$FR)*(1 - primate_ratio))
    )
, byrow = FALSE
)

dimnames(mosq_bite_pref) <- 
  list(
    c("human", "primate", "other")
  , mosq_names)

} else {  ## Malaria
  
num_mosq        <- 1                        

## Mosquito species 
mosq_spec <- list(
  data.frame(genus   = "Anopheles" 
  ,          species = "sp" 
  ))

mosq_names <- c("Anopheles sp")

## Again, requires info about raw mosquito to host ratio. R0 particularly low for Anopheles so bump this up just a tad. (because it is a scalar can divide R0 to directly adjust)
mosq_host_ratio   <- c("Anopheles sp" = 20)

## How mosquitoes respond to urban area and tree cover --- see more extensive commenting in the dengue entry above
mosq_hab_params   <- list(
"Anopheles sp" = c(
  "interc"            = 1 
, "linear_scaling"    = 0
, "quadratic_scaling" = -1
#, "pop_exp"           = pop_exp
, "pop_exp"           = 1
, "oth_scaling"       = 0
, "oth_predictor"     = 30
  )
)

mosq_bite_pref <- matrix(                 
  ncol  = num_mosq
, nrow  = num_hosts
, data  = c(bite.data.an.bite$FR, 1 - bite.data.an.bite$FR)
, byrow = FALSE
)

dimnames(mosq_bite_pref) <- 
  list(
    c("human", "other")
  , mosq_names)

## Anopheles scaling parameters for abundance as a function of forest edge. See pop_dens_kernel.R for how they are used
anoph_scale_1 <- 30
anoph_scale_2 <- 50

}

if (num_mosq != length(mosq_spec)) {
  stop("Number of mosquitoes does not equal mosquito information given")
}

if (num_mosq != length(mosq_host_ratio) | num_mosq != length(mosq_hab_params)) {
  stop("Number of mosquitoes does not equal mosquito information given")
}

## We also need a scaling factor for  "other" hosts, which is a bit difficult to come up with. Since our focus is going to be on human density,
 ## assuming that other host density _per unit of tree cover_ is the reverse of humans is a bit too derived. Here we simply set "others" to be a given 
  ## value and explore variation in human density only 
other_scaling         <- 80

####
## Parameters for transmission probability
####

## Because we have no shot at parameterizing mosquito %by% host species interactions, I am ignoring this for now.
 ## I could conceivably imagine setting up some sort of hyper-prior from which we assume these interactions can be sampled, but that seems
  ## like a bit much for now, especially with two hosts two mosquitoes which is the focus for now

## Not super dynamic, but because this is the "parameters" script, these can be adjusted without much difficulty
 ## The idea here is that with dengue, "other" host titer is modeled as a function of human host titer (because of a lack of data)
  ## For now assume other hosts can't transmit dengue, so make this "NA" (if a number will be used as a number)
oth_host_titer.dengue  <- NA # oth_host_titer.dengue  <- 0.5 

## !! Adjustment to _human_ titer with Yellow Fever because there isn't any data for them. Starting with two low of a yellow fever titer from
 ## humans will produce a quite low R0 estimate. As late as early-mid 20th century yellow fever operated more in an epidemic
 ## fashion, suggesting sustained human-to-human transmission is possible. This doesn't really happen anymore (at least in South 
 ## America any more because of vaccination and vector control. However! it is likely possible human-mosquito-human transmission is quite possible
 ## Aedes aegypti was in fact referred to as the Yellow Fever mosquito at one point in time
  ## Note: What we present here is epidemic potential assuming 100% S population  which isn't the reality for Yellow Fever) 
oth_host_titer.yf      <- 0

## For now assume other hosts can't transmit malaria, so make this "NA" (if a number will be used as a number)
oth_host_titer.malaria <- NA # oth_host_titer.malaria <- 3.5

## Scaling by 10 for biting rate at 1/10 of day to get a finer picture of the titer profiles (makes no real difference, just that using day can result in a bit of a choppy curve)
 ## i.e., basically turning a sum into more of an integral
daily_scaling  <- 10

## Length of infection. Ignored if fitting models from data
inf_days       <- seq(1, 8, 1/daily_scaling)        

## Number of bites each mosquito makes per day over the course of its life (ya, it is more complicated than this (bite, lay, bite etc.),
 ## but as long as we get the average right, we wont get anything too wrong)
  ## There is a potential that Aedes aegypti should be up-scaled some to allow for multiple bites per gonotrophic cycle, but because these
   ## estimates are from observations just take the estimates from the observations for now
if (disease[disease_counter] == "dengue") {
daily_bites    <- c("Aedes aegypti" = 0.555, "Aedes albopictus" = 0.37)
} else if (disease[disease_counter] == "yellow_fever") {
## The exceedingly low bite rate of Haemagogus based on their gonotrophic cycle length produces a very low R0. Since this is simply a scalar, could bump this up to 
 ## get a more "realistic" estimate, though it is pretty unclear what realistic means in this case without any validation so just leave it for now
daily_bites    <- c("Aedes aegypti" = 0.555, "Aedes albopictus" = 0.37, "Haemagogus sp" = 0.122)  
} else {
daily_bites    <- c("Anopheles sp" = 0.55)  
}

## Mark recapture doesn't find mosquitoes after about 10 days, in lab maybe 40 days.
 ## Just in case go overboard and use 60 (at which point probability is *so* close to zero)
mosq_days      <- matrix(
  ncol = num_mosq
, data = rep(seq(1, 60, by = 1), num_mosq)
  )

## Simple exponential decay survival model
if (disease[disease_counter] == "dengue") {
## Data from the lit returns an average of 0.81 for Aedes aegypti, but unpublished experiments by LeBuead and temp-dependent stuff
 ## for aedes aegypti suggests a longer survival, so bump this up a tad. Can come back and explore this in better detail later
surv_prob      <- c("Aedes aegypti" = 0.85, "Aedes albopictus" = 0.91)
## Mosquito max flight distance
flight_dist    <- c("Aedes aegypti" = 200, "Aedes albopictus" = 600)
} else if (disease[disease_counter] == "yellow_fever") {
surv_prob      <- c("Aedes aegypti" = 0.85, "Aedes albopictus" = 0.91, "Haemagogus sp" = 0.93)
flight_dist    <- c("Aedes aegypti" = 200, "Aedes albopictus" = 600, "Haemagogus sp" = 600)  
} else {
surv_prob      <- c("Anopheles sp" = 0.82)
flight_dist    <- c("Anopheles sp" = 600)
}

## Mosquito flight weighting towards its preferred habitat type in proportion to the local values of its preferred habitat type?
mosq_hab_pref  <- FALSE

## Travel distance of the host, only used if host_movement
if (host_movement) {
## Rather arbitrary. Assumes a sick human and "other" host isn't traveling much 
  if (disease[disease_counter] != "yellow_fever") {
travel_dist    <- c("human" = 600, "other" = 400)
  } else {
travel_dist    <- c("human" = 600, "primate" = 400, "other" = 400)  
  }
} else {
travel_dist    <- c("human" = 1, "other" = 1)  
}

## Do the same habitat preference weighting for the hosts (see above note on mosquito weighting)
host_hab_pref  <- TRUE

## which host are we starting with an infection?
 ## For now assume that Dengue and Malaria start human and Yellow Fever starts "other"
which_host <- c(1, 2)
which_adj  <- list(
  c(0, 0)     ## dengue
, c(0, 0, 0)  ## yellow fever
, c(0, 0)     ## malaria
  )
if (starting_host[starting_host_counter] == "human") {
which_adj[[1]][which_host[1]] <- 1
which_adj[[2]][which_host[1]] <- 1
which_adj[[3]][which_host[1]] <- 1
which_pop <- 1
} else {
which_adj[[1]][which_host[2]] <- 1 ## Not actually used for dengue and malaria, just filled in to have the right length
which_adj[[2]][which_host[2]] <- 1
which_adj[[3]][which_host[2]] <- 1  
which_pop <- 2
}

print(which_adj)
