###################################################
### Functions for landscape-disease simulations ###
###################################################

####
## Various functions for simulating physiological responses of hosts and mosquitoes. 
## Many of these are not actually used --- see fit_models.R --- but potentially useful
####

## Ricker function as a non-symmetric option for titer profile (peaks faster than declines from peak)
titer_prof     <- function (day, a, b) {
  a * day * exp(-b * day)
}

## host to mosquito transmission probability follows a logistic function (could divide by k or something if it 
 ## turns out transmission probability never actually reaches 1 (basically does for WNV, so just working with that for now))
h_to_m_inf_prob <- function (titer, a, b) {
  1 / (1 + exp(-(a + b * titer)))
}

## Not too clear on the incubation period in the mosquito, lit seems a bit foggy on it
 ## so for now just assume a less steep logistic 
m_to_h_inf_prob <- function (day, a, b) {
  1 / (1 + exp(-(day - a)/b))
}

## Simple exponential decay survival model
mosq_surv_mod <- function (surv_prob, mosq_days) {
  exp(surv_prob * mosq_days)
}

## Daily survival probability multiplication model (simple to modify for varying survival probability over time if those estimates exist)
mosq_surv_sim <- function (surv_prob, mosq_days) {
  for (i in seq_along(mosq_days)) {
    mosq_days[i]
 cumprod(surv_prob, mosq_days)
  }
}

## Very simple linear relationship between forest cover and host density
dens_forest_cob <- function (a, b, x) {
  a + b * x # where x is forest cover
}

## Assume secondary animal density is a function of forest cover with some scaling factor. Could possibly assume
 ## a non-linear function here, but seems hard to parameterize
alt_dens <- function (trees, scaling) {
  trees * scaling
}

## Flexible function to model all sorts of smooth relationships between mosquitoes and forest and populations.
 ## Assumes some max ratio of mosquitoes to hosts that is affected by the landscape (for now just forest)
  ## see mosq_dens_explore.R to examine this flexible function
mosq_dens <- function (land, pop, ratio, interc, linear_scaling, quadratic_scaling, pop_exp, oth_scaling, oth_predictor) {
  
temp_scaling <- (1  / (1 + exp(-(
   interc + 
   linear_scaling    * land +
   quadratic_scaling * land^2 +
   oth_scaling       * oth_predictor))))

scaling      <- temp_scaling / max(temp_scaling)
mosq_out     <- scaling * (pop ^ pop_exp) * ratio

return(mosq_out)

}
