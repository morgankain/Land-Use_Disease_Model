## Required packages to run this code
needed_packages <- c(
    "plyr"
  , "dplyr"
  , "ggplot2"
  , "magrittr"
  , "scales"
  , "lme4"
  , "merTools"
  , "MASS"
  , "tidyr"
  , "reshape2"
  , "spatialfil"
)

## load packages. Install all packages that return "FALSE"
lapply(needed_packages, require, character.only = TRUE)
