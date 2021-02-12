################################################################
### Convert a geotiff raster into an ''empirical'' landscape ###
################################################################

## Two landscapes from Colombia. 
 ## Note: the manuscript only presents analyses for Bogota (which was the only landscape for which analyses were run in the first place)
land1 <- read.csv("saved_input/bogota_samps.csv")
land5 <- read.csv("saved_input/cali_samps.csv")

## And previously simulated reforestation scenarios from convert_landscape_sim.R (similar to 
 ## sim_landscapes.R these were previously simulated and saved)
sim_land <- readRDS("saved_input/empirical_land_sim_land.Rds")

## Note: For "empirical" there are 8 total landscapes, 4 for each of the two saved landscapes above
 ## For each there are 4 total landscapes 
   ## 1) Baseline  (landscape_coutner == 1 | 5)
   ## 2) large contiguous forest (landscape_coutner == 2 | 6)
   ## 3) highly patchy forest (landscape_coutner == 3 | 7)
   ## 4) upscale all current pixels without changing the orientation  (landscape_coutner == 4 | 8)

names(sim_land) <- c(
  "land1_base", "land1_contiguous", "land1_patchy", "land1_flat"
, "land5_base", "land5_contiguous", "land5_patchy", "land5_flat"
)

land1          <- land1 %>% dplyr::select(-id)
names(land1)   <- c("LAI", "POP", "X", "Y")
land5          <- land5 %>% dplyr::select(-id, -POP1_2)
names(land5)   <- c("LAI", "POP", "X", "Y")

## Population data comes from worldpop.org (unconstrained, 100m resolution, number of people per grid cell which equals number of people per hectare)
 ## Because I sampled at 200m spacing, I need to multiply these values by 4 to get people per 400m^2, which is what I modeled in the simulated landscapes

land1 <- land1 %>% mutate(POP = POP * 4)
land5 <- land5 %>% mutate(POP = POP * 4)

## LAI is measured in EVI, data taken by following the links in this paper: https://rmets.onlinelibrary.wiley.com/doi/pdf/10.1002/gdj3.87
 ## because these are already on a scale of 0-100 I will treat these directly as the values of tree cover that I use in the simulations

land1 <- land1 %>% mutate(LAI = LAI / 100)
land5 <- land5 %>% mutate(LAI = LAI / 100)


## Convert the long form data to matrices
land1.pop <- matrix(data = land1$POP, nrow = length(unique(land1$X)), ncol = length(unique(land1$Y)))
land1.lai <- matrix(data = land1$LAI, nrow = length(unique(land1$X)), ncol = length(unique(land1$Y)))

land5.pop <- matrix(data = land5$POP, nrow = length(unique(land5$X)), ncol = length(unique(land5$Y)))
land5.lai <- matrix(data = land5$LAI, nrow = length(unique(land5$X)), ncol = length(unique(land5$Y)))

## determine the dimensions of the extracted matrices
if (landscape_counter <= 4) {
  matx <- length(unique(land1$X))
  maty <- length(unique(land1$Y))
} else {
  matx <- length(unique(land5$X))
  maty <- length(unique(land5$Y))
}

## Some of the values in these matrices are NA. Find those and replace the values with the mean of the nearby values
land1.pop.na <- which(is.na(land1.pop), arr.ind = T)
for (i in 1:nrow(land1.pop.na)) {
land1.pop[land1.pop.na[i, 1], land1.pop.na[i, 2]] <- mean(land1.pop[
  (land1.pop.na[i, 1] - 1):(land1.pop.na[i, 1] + 1)
, (land1.pop.na[i, 2] - 1):(land1.pop.na[i, 2] + 1)
  ]
  , na.rm = T)
}

land5.pop.na <- which(is.na(land5.pop), arr.ind = T)
for (i in 1:nrow(land5.pop.na)) {
land5.pop[land5.pop.na[i, 1], land5.pop.na[i, 2]] <- mean(land5.pop[
  (land5.pop.na[i, 1] - 1):(land5.pop.na[i, 1] + 1)
, (land5.pop.na[i, 2] - 1):(land5.pop.na[i, 2] + 1)
  ]
  , na.rm = T) 
}

## Create the empty landscape containers
landscape[[landscape_counter]] <- array(
    data = 0
  , dim = c(
     matx 
   , maty 
   , 2 + num_hosts + num_mosq
  ))

## Name the empty landscape containers
if (disease[disease_counter] != "yellow_fever") {
dimnames(landscape[[landscape_counter]]) <- list(
  NULL
, NULL
, c("urban", "human", "forest", "other", mosq_names)
)
} else {
dimnames(landscape[[landscape_counter]]) <- list(
  NULL
, NULL
, c("urban", "human", "forest", "primate", "other", mosq_names)
)  
}

## A few things used in sim_landscapes.R that are needed for pop_dens_kernel.R
landscape_predictors <- c("human_population", "tree_cover")
num_land             <- length(landscape_predictors)
which_tree           <- grep("tree", landscape_predictors)

#####
## Fill the landscape with either the baseline map extracted from QGIS or a landscape with simulated additional forest cover
#####

## Grab the appropriate human population
if (landscape_counter <= 4) {
landscape[[landscape_counter]][, , "human"]  <- get(paste("land", "1", ".", "pop", sep = ""))
landscape[[landscape_counter]][, , "urban"]  <- get(paste("land", "1", ".", "pop", sep = "")) 

} else {
landscape[[landscape_counter]][, , "human"]  <- get(paste("land", "5", ".", "pop", sep = ""))   
landscape[[landscape_counter]][, , "urban"]  <- get(paste("land", "5", ".", "pop", sep = ""))  
}
landscape[[landscape_counter]][, , "urban"]  <- landscape[[landscape_counter]][, , "urban"] / max(landscape[[landscape_counter]][, , "urban"])

landscape[[landscape_counter]][, , "forest"] <- sim_land[[landscape_counter]]

###### ggplots of the landscapes 

land1.pop.gg        <- melt(land1.pop)
names(land1.pop.gg) <- c("X", "Y", "val")
land1.pop.gg        <- land1.pop.gg %>% mutate(land = "pop")

land1.lai.gg        <- melt(land1.lai)
names(land1.lai.gg) <- c("X", "Y", "val")
land1.lai.gg        <- land1.lai.gg %>% mutate(land = "lai")

land5.pop.gg          <- melt(land5.pop)
names(land5.pop.gg)   <- c("X", "Y", "val")
land5.pop.gg          <- land5.pop.gg %>% mutate(land = "pop")

land5.lai.gg          <- melt(land5.lai)
names(land5.lai.gg)   <- c("X", "Y", "val")
land5.lai.gg          <- land5.lai.gg %>% mutate(land = "lai")

gg.land.1 <- ggplot(land1.pop.gg, aes(X, rev(Y), z = val)) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(fill = val)) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(title = "Population")) +
#  scale_x_continuous(breaks = c(0, 3, 6, 9 , 12)) +
#  scale_y_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  )

gg.land.2 <- ggplot(land1.lai.gg, aes(X, rev(Y), z = val)) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(fill = val)) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(title = "EVI Leaf Area Index")) +
#  scale_x_continuous(breaks = c(0, 3, 6, 9 , 12)) +
#  scale_y_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  )

gg.land.3 <- ggplot(land5.pop.gg, aes(X, rev(Y), z = val)) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(fill = val)) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(title = "Population")) +
#  scale_x_continuous(breaks = c(0, 3, 6, 9 , 12)) +
#  scale_y_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  )

gg.land.4 <- ggplot(land5.lai.gg, aes(X, rev(Y), z = val)) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(fill = val)) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(title = "EVI Leaf Area Index")) +
#  scale_x_continuous(breaks = c(0, 3, 6, 9 , 12)) +
#  scale_y_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  )

## Double back and make sure cells with exactly zero trees dont gain any trees because these are likely cells of water
if (landscape_counter != 1 & landscape_counter != 5) {
 
  if (landscape_counter < 5) {
    landscape[[landscape_counter]][,,"forest"][which(get(paste("land", "1", ".", "lai", sep = "")) == 0)] <- 0
  } else {
    landscape[[landscape_counter]][,,"forest"][which(get(paste("land", "5", ".", "lai", sep = "")) == 0)] <- 0
  }
   
}


