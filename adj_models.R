#########################################################################################
### Predictions from fitted models to physiological responses of hosts and mosquitoes ###
#########################################################################################

## Use the fitted models to predict responses for the given diseases and mosquitoes for the loop iteration

if (disease[disease_counter] == "dengue") {
  
## Fine to make this super long because titer will decay to 0 and have no effect
pred.day             <- seq(-10, 15, by = 1/daily_scaling)
host_titer           <- array(dim = c(length(pred.day), num_hosts))
dimnames(host_titer) <- list(pred.day, c("human", "other"))

## Predict titer response from the fitted model -- with no uncertainty :(
host_titer[, "human"] <- 
  exp(predict(host_titer.mod.D
    , newdata = data.frame(
      Day = pred.day)))

## Major assumption: Quite a lack of data for host responses, so assume titer for Dengue in "other" is some tiny fraction
 ## of what it is in humans. That is, assume _on average_ other hosts are really bad at transmitting. Not exactly the same
  ## as assuming 0 titer in many but reasonable titer for infection in a few, but since collapsing to "other" average it out
   ## (yes, some small issue with non-linearity of probability of infection)
## There are plenty of citations show that Dengue seems to be able to be transmitted by a number of other hosts, 
 ## but clearly with _much_ less consistency than through humans
if (is.na(oth_host_titer.dengue)) {
host_titer[, "other"] <- 0 
} else {
host_titer[, "other"] <- exp(log(host_titer[, "human"]) - oth_host_titer.dengue)
}

h_to_m_trans  <- array(dim = c(length(pred.day), num_hosts, num_mosq))

dimnames(h_to_m_trans) <- list(
  pred.day
, c("human", "other")
, c("aedes aegypti", "aedes albopictus"))

## For all host to mosquito transmission measures, just use day 14 which is _A_ standard for checking if a mosquito got infected.
 ## There is some variation here, but day 14 is by far the most common
  ## (We acknowledge that it is more complicated than this, as shown by Salazar et al. 2007, BMC Microbiology, Figure 7)
h_to_m_trans[, "human", "aedes aegypti"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "human"]
    , Species = "aegypti"
    , Virus   = "Dengue"
      )
    , re.form = NA
      ))

h_to_m_trans[, "other", "aedes aegypti"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "other"]
    , Species = "aegypti"
    , Virus   = "Dengue"
      )
    , re.form = NA
      ))

h_to_m_trans[, "human", "aedes albopictus"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "human"]
    , Species = "albopictus"
    , Virus   = "Dengue"
      )
    , re.form = NA
      ))

h_to_m_trans[, "other", "aedes albopictus"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "other"]
    , Species = "albopictus"
    , Virus   = "Dengue"
      )
    , re.form = NA
      ))

## In the absence of any information on GxG for mosquito-to-host transmission,
 ## assume mosquito transmission | bite on S hosts is identical by host type (so don't need the extra array dimension)
m_to_h_trans  <- array(dim = c(nrow(mosq_days), num_mosq))

dimnames(m_to_h_trans) <- list(
  mosq_days[, 1]
, c("aedes aegypti", "aedes albopictus")
)

## Major assumption: Don't have enough data for transmission so just fitting one model to Dengue and Yellow Fever and using the same
 ## prediction for both diseases. AND ALSO for Malaria, which is potentially a bit crazy, but not sure what to do for now
m_to_h_trans[, "aedes aegypti"] <- 
  plogis(predict(m_to_h_trans.mod
    , newdata   = data.frame(
      Day       = mosq_days[, 1]
## Tracking the titer in the host for each mosquito for mosquito transmission would require an additional array dimension for all mosquitoes
 ## from all home cells and would greatly increase RAM requirements to run the model and computational time. Instead we just assume
  ## mosquitoes pick up an average dose
    , Titer.cen = 0 
    , Species = "aegypti"
      )
    , re.form = NA
      ))

m_to_h_trans[, "aedes albopictus"] <- 
  plogis(predict(m_to_h_trans.mod
    , newdata   = data.frame(
      Day       = mosq_days[, 1]
    , Titer.cen = 0 ## Centered
    , Species = "albopictus"
      )
    , re.form = NA
      ))

## Mosquito survival, standard method but that doesn't make it any less loose and not super accurate for how 
 ## survival and biting behavior are interacting here vis gonotrophic cycle, development etc.
mosq_surv      <- exp(matrix(rep(-(1-surv_prob), each = nrow(mosq_days)), ncol = 2, byrow = F)) ^ mosq_days
m_to_h_trans_s <- m_to_h_trans * mosq_surv

} else if (disease[disease_counter] == "yellow_fever") {
  ## reduced notes here. See notes in disease[disease_counter] == "dengue"

pred.day             <- seq(1, 10, by = 1/daily_scaling)
host_titer           <- array(dim = c(length(pred.day), num_hosts))
dimnames(host_titer) <- list(pred.day, c("human", "primate", "other"))

## Opposite order for yellow fever (fit to primate first, assumption from there for human (true "others" are sinks for infection))
host_titer[, "primate"] <- 
  exp(predict(host_titer.mod.YF
    , newdata = data.frame(
      Day = pred.day)))

## Major assumption: one major concern here is that humans seem to have a very long infection period for Dengue.
 ## Maybe that is the same for Yellow Fever? (i.e. animal hosts seem[?] to manifest titer for a shorter period where humans 
  ## manifest titer prior to symptom onset -- minimal data here so unclear if this is just what we have happened to observe and not widely applicable)
host_titer[, "human"]  <- exp(log(host_titer[, "primate"]) - oth_host_titer.yf)

host_titer[, "other"] <- 0

h_to_m_trans           <- array(dim = c(length(pred.day), num_hosts, num_mosq))

dimnames(h_to_m_trans) <- list(
  pred.day
, c("human", "primate", "other")
, c("aedes aegypti", "aedes albopictus", "haemagogus sp"))

h_to_m_trans[, "human", "aedes aegypti"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "human"]
    , Species = "aegypti"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

h_to_m_trans[, "primate", "aedes aegypti"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "primate"]
    , Species = "aegypti"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

h_to_m_trans[, "other", "aedes aegypti"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "other"]
    , Species = "aegypti"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

h_to_m_trans[, "human", "aedes albopictus"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "human"]
    , Species = "albopictus"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

h_to_m_trans[, "primate", "aedes albopictus"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "primate"]
    , Species = "albopictus"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

h_to_m_trans[, "other", "aedes albopictus"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "other"]
    , Species = "albopictus"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

h_to_m_trans[, "human", "haemagogus sp"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "human"]
    , Species = "albopictus"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

h_to_m_trans[, "primate", "haemagogus sp"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "primate"]
    , Species = "albopictus"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

h_to_m_trans[, "other", "haemagogus sp"] <- 
  plogis(predict(h_to_m_trans.mod.DYF
    , newdata = data.frame(
      Day     = 14
    , Titer   = host_titer[, "other"]
    , Species = "albopictus"
    , Virus   = "Yellow Fever"
      )
    , re.form = NA
      ))

m_to_h_trans  <- array(dim = c(nrow(mosq_days), num_mosq))

dimnames(m_to_h_trans) <- list(
  mosq_days[, 1]
, c("aedes aegypti", "aedes albopictus", "haemagogus sp")
)

m_to_h_trans[, "aedes aegypti"] <- 
  plogis(predict(m_to_h_trans.mod
    , newdata   = data.frame(
      Day       = mosq_days[, 1]
    , Titer.cen = 0 ## Centered
    , Species = "aegypti"
      )
    , re.form = NA
      ))

m_to_h_trans[, "aedes albopictus"] <- 
  plogis(predict(m_to_h_trans.mod
    , newdata   = data.frame(
      Day       = mosq_days[, 1]
    , Titer.cen = 0 ## Centered
    , Species = "albopictus"
      )
    , re.form = NA
      ))

m_to_h_trans[, "haemagogus sp"] <- 
  plogis(predict(m_to_h_trans.mod.YF.H
    , newdata   = data.frame(
      day       = mosq_days[, 1]
      )
      ))

## Exponential rates survival model
mosq_surv      <- exp(matrix(rep(-(1-surv_prob), each = nrow(mosq_days)), ncol = num_mosq, byrow = F)) ^ mosq_days
m_to_h_trans_s <- m_to_h_trans * mosq_surv
  
} else if (disease[disease_counter] == "malaria") {
  
pred.day             <- seq(1, 19, by = 1/daily_scaling)
host_titer           <- array(dim = c(length(pred.day), num_hosts))
dimnames(host_titer) <- list(pred.day, c("human", "other"))

host_titer[, "human"] <- 
  10^(predict(host_titer.mod.M
    , newdata = data.frame(
      Day = pred.day)))

## Bit of a loose way to model "other" hosts generally being quite bad for malaria propogation, but w/e for now...
if (is.na(oth_host_titer.malaria)) {
host_titer[, "other"] <- 0.01 ## need to take log so cant = 0
} else {
host_titer[, "other"] <- 10^(log10(host_titer[, "human"]) - oth_host_titer.malaria)  
}

h_to_m_trans  <- array(dim = c(length(pred.day), num_hosts, num_mosq))

dimnames(h_to_m_trans) <- list(
  pred.day
, c("human", "other")
, c("anopheles"))
  
## Choice here to focus just on Anopheles darlingi, the main species in the Americas. A bit at odds with the rest of the 
 ## choices for Malaria because we mix species, but in this case the other species' data causes a pretty odd infection probability
  ## curve because of a bunch of data from gambiae (see supp plot in the paper)
h_to_m_trans[, "human", "anopheles"] <- 
  plogis(predict(h_to_m_trans.mod.M
    , newdata = data.frame(
      log10titer   = log10(host_titer[, "human"])
    , Species      = "darlingi"
      )
    , re.form = NA
      ))

h_to_m_trans[, "other", "anopheles"] <- 
  plogis(predict(h_to_m_trans.mod.M
    , newdata = data.frame(
      log10titer   = log10(host_titer[, "other"])
    , Species      = "darlingi"
      )
    , re.form = NA
      ))

m_to_h_trans  <- array(dim = c(nrow(mosq_days), num_mosq))

dimnames(m_to_h_trans) <- list(
  mosq_days[, 1]
, c("anopheles")
)

m_to_h_trans[, "anopheles"] <-  matrix(
  data = 
  predict(m_to_h_trans.mod.M
    , newdata   = expand.grid(
      Day       = mosq_days[, 1]
    , Temperature = 23                  
      )
      , type = "response")
  , nrow = length(mosq_days[, 1])
)

mosq_surv      <- exp(-(1-surv_prob)) ^ mosq_days
m_to_h_trans_s <- mosq_surv * m_to_h_trans

}
