######################################################
### Summarize and combine results from simulations ###
######################################################

source("needed_packages.R")
source("ggplot_theme.R")
source("functions.R")

##########
## NOTE: this code is set up to load, parse, summarize and combine a series of saved Rds files, each one being a single model run
## This script is set up this way because an individual run (one set of parameters on one landscape) takes about 12 hours. 
## Jobs were sent to a computing cluster and returned one parameter set at a time. The code below summarizes and compiles all of these individual model runs.
## There are over two hundred individual Rds files so they are not uploaded to github or supplemental material. Thus, much of this code may
## make relatively little sense, but it is nonetheless included in this repo for completeness.
##########
## ALSO NOTE: In manuscript_figures_details.R the list of the parameter values used in the simulations to create each figure are listed
##########
## Finally, please contact me with any questions about this script or for specific saved Rds files
#########

## This first section contains a series of parameters that control how the saved output is summarized in order to create data frames for plotting
 ## Because of a series of iterations of runs on the cluster and a combination of slightly older and newer structures by
  ## which output was saved, a number of parameters are needed to 

sim_landscape   <- TRUE
old_FOI_version <- FALSE  ## TRUE = Prior to an update on Jan 7 that saves the different paths of infection to humans from each mosquito

whichland     <- c(
  "land1_base", "land1_contiguous", "land1_patchy", "land1_flat"
, "land5_base", "land5_contiguous", "land5_patchy", "land5_flat"
)
date.out      <- "2021-01-09"
disease.out   <- c("dengue","yellow_fever","malaria")
start_inf.out <- c("human", "primate", "other")

## Different densities that have been used, and will be needed
 
rev(c(5, 10, 20, 40, 80, 160, 320))                          ## main runs for each disease 
  c(10, 2)                                                   ## extra runs to add to the main runs
rev(seq(10, 400, by = 20))[-20]                              ## dengue popdens
rev(seq(2, 26, length = 7))                                  ## extra dengue popdens runs
c(rev(seq(30, 400, by = 20)), rev(seq(2, 26, length = 7)))   ## yellow fever popdens

## landscapes always follow:
(1 - seq(0, 0.92, by = 0.02))[seq(2, 47, length = 10)]

land.dens.out <- expand.grid(
   land.out = 3 # seq(1, 10, 1) # c(3, 5, 7) # seq(1, 10, 1)
# , dens.out = seq(1, 19, by = 1)[-c(1, 4, 7)] # c(1, 3, 5, 7)
# , dens.out = rev(seq(10, 400, by = 20))[-c(1, 4, 7, 20)]
#  , dens.out = c(rev(seq(10, 400, by = 20))[-20][-c(1, 4, 7)], 26, 22, 18, 14, 10, 6)
  , dens.out = c(rev(seq(30, 400, by = 20)), rev(seq(2, 26, length = 7)))[-26]
#  , dens.out = c(2, 5, 10, 20, 80, 320)
# , dens.out = c(20)
 , dis.out  = 2 # c(1,2,3) # c(1, 2, 3) 
 , host.out = c(1, 2)   # c(1, 2)
 , exp.out  = c(1) # seq(0.4, 2, by = 0.2)
  )

use.variable_version <- FALSE ## Do a mix of summarizing of old and new runs
fig.1                <- FALSE ## because figure 1 mixes old and new runs need to do a bit of special summarizing

if (sim_landscape == FALSE) {
land.dens.out <- land.dens.out %>% filter(!(host.out == 2 & dis.out != 2))
}

travel.out  <- 600
exp.out     <- 1

run_num <- 1

## What landscapes level autocorrelation was used
het <- (1 - seq(0, 0.92, by = 0.02))[seq(2, 47, length = 10)]

# check_aedes_dens <- numeric(nrow(land.dens.out))

for (j in 1:nrow(land.dens.out)) {
  
if (use.variable_version) {
if ((land.dens.out[j, "dens.out"] !=2) & (land.dens.out[j, "dens.out"] != 10)) {
  old_FOI_version <- TRUE
} else {
  old_FOI_version <- FALSE
}
}
  
if (sim_landscape) {
  
name.out <- paste(paste(
  "landscape.out"
 , sim_landscape
 , land.dens.out[j, "dens.out"]
 , disease.out[land.dens.out[j, "dis.out"]]
 , land.dens.out[j, "land.out"]
 , travel.out
 , land.dens.out[j, "exp.out"]
 , "Final"
 , sep = "_")
  , ".Rds", sep = "")  

name.out <- paste(
  "saved_output"
, "simT"
, disease.out[land.dens.out[j, ]$dis.out]
#, "exponent"
, "pop_dens"
, name.out
, sep = "/"
)

} else {
  
name.out <- paste(paste(
  "landscape.out"
 , sim_landscape
 , land.dens.out[j, "land.out"]
 , disease.out[land.dens.out[j, "dis.out"]]
 , land.dens.out[j, "host.out"]
 , travel.out
 , land.dens.out[j, "exp.out"]
 , "Final"
 #, "onesource"
 , sep = "_")
  , ".Rds", sep = "")


name.out <- paste(
  "saved_output"
, "simF"
, disease.out[land.dens.out[j, ]$dis.out]
#, "exponent"
#, "pop_dens"
, name.out
, sep = "/"
)
}

print(name.out)
  
landscape.out.top <- readRDS(name.out)  

landscape.out     <- landscape.out.top[[
    paste(disease.out[land.dens.out[j, "dis.out"]]
  , land.dens.out[j, "land.out"]
  , start_inf.out[land.dens.out[j, "host.out"]]
  , sep = "_")]]

# check_aedes_dens[j] <- mean(landscape.out[["landscape"]][, , "Aedes aegypti"])

if (!is.null(landscape.out)) {
  
## Add the landscape characteristics to the output for plotting purposes
current_landscape        <- melt(landscape.out$landscape[,,c(1,3)])
names(current_landscape) <- c("X", "Y", "land_type", "value")
current_landscape        <- current_landscape %>% pivot_wider(names_from = "land_type", values_from = "value")
source("forest_var_calc.R")
current_landscape        <- current_landscape %>% left_join(., temp_anoph_landscape, by = c("X", "Y"))

####
## One full generation of infection
####
if (disease.out[land.dens.out[j, "dis.out"]] != "yellow_fever") {
R0.home.s                <- array(data = 0, dim = c(dim(landscape.out$R0.h)[1], dim(landscape.out$R0.h)[2], 3))
dimnames(R0.home.s)[[3]] <- c("Human", "Other", "Both")
} else {
R0.home.s                <- array(data = 0, dim = c(dim(landscape.out$R0.h)[1], dim(landscape.out$R0.h)[2], 4))
dimnames(R0.home.s)[[3]] <- c("Human", "Primate", "Other", "Both")  
}
R0.home.s[,,"Human"]     <- landscape.out$R0.h
R0.home.s[,,"Other"]     <- landscape.out$R0.o
R0.home.s[,,"Both"]      <- landscape.out$R0
if (disease.out[land.dens.out[j, "dis.out"]] == "yellow_fever") {
R0.home.s[,,"Primate"]   <- landscape.out$R0.p  
}
R0.home.s.gg             <- melt(R0.home.s)
names(R0.home.s.gg)      <- c("X", "Y", "Host", "R0")

R0.home.s.gg <- left_join(R0.home.s.gg, current_landscape, by = c("X", "Y"))

R0.home.s.gg.t <- R0.home.s.gg %>%
  mutate(
    disease       = disease.out[land.dens.out[j, "dis.out"]]
  , land          = land.dens.out[j, "land.out"]
  , dens          = land.dens.out[j, "dens.out"]
  , starting_host = start_inf.out[land.dens.out[j, "host.out"]]
  , exponent      = land.dens.out[j, "exp.out"]
    )

if (!sim_landscape) {
R0.home.s.gg.t <- R0.home.s.gg.t %>% mutate(which_landscape = whichland[land])
}

####
## Just host to mosquito transmission (infection of mosquitoes from that first infected host).
####
h_m_mat.home.s           <- landscape.out$all_mosquitos  
h_m_mat.home.s.gg        <- melt(h_m_mat.home.s)
names(h_m_mat.home.s.gg) <- c("X", "Y", "H_to_M")

#if (disease.out[land.dens.out[j, "dis.out"]] == "dengue") {
#  
#h_m_mat.home.s.m1           <- landscape.out$num_inf_mosquito.aegypti 
#h_m_mat.home.s.m1.gg        <- melt(h_m_mat.home.s.m1)
#names(h_m_mat.home.s.m1.gg) <- c("X", "Y", "H_to_M_aegypti")  

#h_m_mat.home.s.m2           <- landscape.out$num_inf_mosquito.albopictus  
#h_m_mat.home.s.m2.gg        <- melt(h_m_mat.home.s.m2)
#names(h_m_mat.home.s.m2.gg) <- c("X", "Y", "H_to_M_albopictus")

#h_m_mat.home.s.m <- left_join(h_m_mat.home.s.m1.gg, h_m_mat.home.s.m2.gg)
#
#h_m_mat.home.s.gg <- left_join(h_m_mat.home.s.gg, h_m_mat.home.s.m)
#}

h_m_mat.home.s.gg <- left_join(h_m_mat.home.s.gg, current_landscape, by = c("X", "Y"))

h_m_mat.home.s.gg.t <- h_m_mat.home.s.gg %>%
  mutate(
    disease = disease.out[land.dens.out[j, "dis.out"]]
  , land    = land.dens.out[j, "land.out"]
  , dens    = land.dens.out[j, "dens.out"]
  , starting_host = start_inf.out[land.dens.out[j, "host.out"]]
  , exponent      = land.dens.out[j, "exp.out"]
    )

if (!sim_landscape) {
h_m_mat.home.s.gg.t <- h_m_mat.home.s.gg.t %>% mutate(which_landscape = whichland[land])
}

####
## FOI on each cell
####

if (old_FOI_version) {

FOI_on_h.s        <- landscape.out$FOI_on.h
FOI_on_h.s        <- melt(FOI_on_h.s)
names(FOI_on_h.s) <- c("X", "Y", "FOI_on_H")

FOI_on_o.s        <- landscape.out$FOI_on.o
FOI_on_o.s        <- melt(FOI_on_o.s)
names(FOI_on_o.s) <- c("X", "Y", "FOI_on_O")

if (disease.out[land.dens.out[j, "dis.out"]] != "yellow_fever") {
  
## placeholder so rbind below works for all diseases
FOI_on_p.s          <- FOI_on_o.s
names(FOI_on_p.s)   <- c("X", "Y", "FOI_on_P")
FOI_on_p.s$FOI_on_P <- NA

} else {
  
#FOI_on_p.s       <- apply(landscape.out$FOI_on.p, 3, sum)
#FOI_on_p.s       <- matrix(data = FOI_on_p.s, nrow = dim(landscape.out$FOI_on.p)[1], ncol = dim(landscape.out$FOI_on.p)[2])
FOI_on_p.s        <- landscape.out$FOI_on.p
FOI_on_p.s        <- melt(FOI_on_p.s)
names(FOI_on_p.s) <- c("X", "Y", "FOI_on_P")

}

FOI_on.s          <- left_join(FOI_on_h.s, FOI_on_p.s, by = c("X", "Y")) %>%
  left_join(.,  FOI_on_o.s, by = c("X", "Y")) %>%
  left_join(.,  current_landscape, by = c("X", "Y"))

FOI_on.s <- FOI_on.s %>%
  mutate(
    disease = disease.out[land.dens.out[j, "dis.out"]]
  , land    = land.dens.out[j, "land.out"]
  , dens    = land.dens.out[j, "dens.out"]
  , starting_host = start_inf.out[land.dens.out[j, "host.out"]]
  , exponent      = land.dens.out[j, "exp.out"]
    )

if (!sim_landscape) {
FOI_on.s <- FOI_on.s %>% mutate(which_landscape = whichland[land])
}

} else {
 
FOI_on_h.s        <- landscape.out$FOI_on.h
FOI_on_h.s        <- melt(FOI_on_h.s)
names(FOI_on_h.s) <- c("X", "Y", "Mosquito", "FOI_on_H")

## Create an extra column that his the sum of the two mosquitoes and add it to the bottom of the data frame
FOI_on_h.s.s      <- FOI_on_h.s %>% group_by(X, Y) %>% summarize(FOI_on_H = sum(FOI_on_H)) %>%
  mutate(Mosquito = "all") %>% relocate(FOI_on_H, .after = Mosquito) 

FOI_on_h.s <- rbind(FOI_on_h.s, FOI_on_h.s.s)

FOI_on_o.s        <- landscape.out$FOI_on.o
FOI_on_o.s        <- melt(FOI_on_o.s)
names(FOI_on_o.s) <- c("X", "Y", "Mosquito", "FOI_on_O")

## Create an extra column that his the sum of the two mosquitoes and add it to the bottom of the data frame
FOI_on_o.s.s      <- FOI_on_o.s %>% group_by(X, Y) %>% summarize(FOI_on_O = sum(FOI_on_O)) %>%
  mutate(Mosquito = "all") %>% relocate(FOI_on_O, .after = Mosquito) 

FOI_on_o.s <- rbind(FOI_on_o.s, FOI_on_o.s.s)

if (disease.out[land.dens.out[j, "dis.out"]] != "yellow_fever") {
  
## placeholder so rbind below works for all diseases
FOI_on_p.s          <- FOI_on_o.s
names(FOI_on_p.s)   <- c("X", "Y", "Mosquito", "FOI_on_P")
FOI_on_p.s$FOI_on_P <- NA

### Create an extra column that his the sum of the two mosquitoes and add it to the bottom of the data frame
#FOI_on_p.s.s      <- FOI_on_p.s %>% filter(Mosquito == 1) %>% mutate(Mosquito = "all")
#FOI_on_p.s        <- rbind(FOI_on_p.s, FOI_on_p.s.s)

} else {
  
FOI_on_p.s        <- landscape.out$FOI_on.p
FOI_on_p.s        <- melt(FOI_on_p.s)
names(FOI_on_p.s) <- c("X", "Y", "Mosquito", "FOI_on_P")

## Create an extra column that his the sum of the two mosquitoes and add it to the bottom of the data frame
FOI_on_o.p.s      <- FOI_on_p.s %>% group_by(X, Y) %>% summarize(FOI_on_P = sum(FOI_on_P)) %>%
  mutate(Mosquito = "all") %>% relocate(FOI_on_P, .after = Mosquito) 

FOI_on_p.s <- rbind(FOI_on_p.s, FOI_on_o.p.s)

}

if (old_FOI_version) {

FOI_on.s          <- left_join(FOI_on_h.s, FOI_on_p.s, by = c("X", "Y")) %>%
  left_join(.,  FOI_on_o.s, by = c("X", "Y")) %>%
  left_join(.,  current_landscape, by = c("X", "Y"))

} else {
  
FOI_on.s          <- left_join(FOI_on_h.s, FOI_on_p.s, by = c("X", "Y", "Mosquito")) %>%
  left_join(.,  FOI_on_o.s, by = c("X", "Y", "Mosquito")) %>%
  left_join(.,  current_landscape, by = c("X", "Y"))  

## For Figure 1 just some over mosquitoes
if (fig.1) {
FOI_on.s <- FOI_on.s %>% group_by(X, Y) %>% filter(Mosquito == "all") %>% dplyr::select(-Mosquito)
}
  
}

FOI_on.s <- FOI_on.s %>%
  mutate(
    disease = disease.out[land.dens.out[j, "dis.out"]]
  , land    = land.dens.out[j, "land.out"]
  , dens    = land.dens.out[j, "dens.out"]
  , starting_host = start_inf.out[land.dens.out[j, "host.out"]]
  , exponent      = land.dens.out[j, "exp.out"]
    )

if (!sim_landscape) {
FOI_on.s <- FOI_on.s %>% mutate(which_landscape = whichland[land])
}

}

if (run_num == 1) {

R0.home.s.gg.t.f      <- R0.home.s.gg.t
h_m_mat.home.s.gg.t.f <- h_m_mat.home.s.gg.t
FOI_on.s.f            <- FOI_on.s
run_num               <- run_num + 1

} else {
 
R0.home.s.gg.t.f      <- rbind(R0.home.s.gg.t.f, R0.home.s.gg.t) 
h_m_mat.home.s.gg.t.f <- rbind(h_m_mat.home.s.gg.t.f, h_m_mat.home.s.gg.t)
FOI_on.s.f            <- rbind(FOI_on.s.f, FOI_on.s)
run_num               <- run_num + 1 
}

print(
  paste("----------------------------------------------------------------------------------Run:"
  , disease.out[land.dens.out[j, "dis.out"]]
  , "+", start_inf.out[land.dens.out[j, "host.out"]]
  , "for landscape", land.dens.out[j, "land.out"])
  )

} else {
  
print(
  paste("NOT run:"
    , disease.out[land.dens.out[j, "dis.out"]]
    , "+", start_inf.out[land.dens.out[j, "host.out"]])
  )
  
}

}

saveRDS(list(
  R0.home.s.gg.t.f      = R0.home.s.gg.t.f
, h_m_mat.home.s.gg.t.f = h_m_mat.home.s.gg.t.f
, FOI_on.s.f            = FOI_on.s.f
 )
, paste("saved_output/landscape_results_popdens_"
,  Sys.Date()
,  ".Rds"
,  sep = "")
  )

#####
## Load previously sorted and saved output + a bit of organizing
#####

all_out               <- readRDS(paste("saved_output/landscape_results_empirical_"
,  "2021-01-04"
#,  "2021-01-11"
,  ".Rds"
,  sep = ""))
R0.home.s.gg.t.f      <- all_out[[1]]
h_m_mat.home.s.gg.t.f <- all_out[[2]]
FOI_on.s.f            <- all_out[[3]]
rm(all_out)

#####
## Components of R0 and R0 of a disease if it begins with one infection in grid cell i, j 
#####

if (sim_landscape) {

R0.home.s.gg.t.f <- R0.home.s.gg.t.f %>% mutate(
  land = mapvalues(
    land
  , from = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  , to   = het
)
#, dens = mapvalues(
#  dens 
#  , from = c(1, 2, 3, 4, 5, 6, 7)
#  , to   = rev(c(5, 10, 20, 40, 80, 160, 320))
#)
)

R0.home.s.gg.t.f <- R0.home.s.gg.t.f %>% mutate(
  dens = dens * 25 / 2
)

FOI_on.s.f <- FOI_on.s.f %>% mutate(
  land = mapvalues(
    land
  , from = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  , to   = het
)
#, dens = mapvalues(
#  dens 
#  , from = c(1, 2, 3, 4, 5, 6, 7)
#  , to   = rev(c(5, 10, 20, 40, 80, 160, 320))
# , from = seq(1, 19)[-c(1, 4, 7)]
# , to   = rev(seq(10, 400, by = 20))[-c(1, 4, 7, 20)]
#)
)

FOI_on.s.f <- FOI_on.s.f %>% mutate(
  dens = dens * 25 / 2
)

}

R0.home.s.gg.t.f <- R0.home.s.gg.t.f %>% mutate(
  disease = mapvalues(disease
  , from = c("dengue", "malaria", "yellow_fever")
  , to   = c("Dengue", "Malaria", "Yellow Fever"))
)

R0.home.s.gg.t.f <- R0.home.s.gg.t.f %>% mutate(
  X = X * 200 / 1000
, Y = Y * 200 / 1000
)

#test_mod <- lm(
#   R0 ~ (human_population + tree_cover + forest_mean + forest_var + urban_mean + urban_var) * (land + dens) * disease
# , data = R0.home.s.gg.t.f); summary(test_mod)

FOI_on.s.f <- FOI_on.s.f %>% mutate(
  disease = mapvalues(disease
  , from = c("dengue", "malaria", "yellow_fever")
  , to   = c("Dengue", "Malaria", "Yellow Fever"))
)

FOI_on.s.f <- FOI_on.s.f %>% mutate(
  X = X * 200 / 1000
, Y = Y * 200 / 1000
)

## View a given landscape
source("explore_results_landplot.R")
