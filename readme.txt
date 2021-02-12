Code for running the model presented in: "[Placeholder Title -- Landscape Disease Model]" submitted to XXXX on XXXX, XX, 2021.
All code and data needed to run the model is included in this repo. Model output for the figures presented in the manuscript are not included here because of their size. For these saved model results contact me.


---------------
Sub directories
---------------

/disease_parameters -- data from the lit needed for the model
/figure_code        -- R scripts for plotting of saved output (code for manuscript plots primarily)
/saved_input        -- derived and saved inputs for the model (e.g., simulated landscapes)
/saved_output       -- location for completed simulations

---------
R scripts
---------

top_level_script.R      -- The first/main script to load to run the model. All other needed scripts are sourced from within this script in the order in which they are needed. Open this, and follow the commenting to set up and run the model

**In alphabetical order, not in the order of use / sourcing**

adj_models.R               -- Predictions from fitted models to physiological responses of hosts and mosquitoesconvert_landscapes_sim.R   -- Simulate reforestation on the empirical landscapeconvert_landscapes.R       -- Convert a geotiff raster into an ''empirical'' landscape
fit_models.R               -- Fit models to physiological responses of hosts and mosquitoesfunctions.R                -- Functions for landscape-disease simulationsggplot_theme.R             -- Custom theme for beautified ggplotshost_movement_setup.R      -- For R0 calculation need to figure out where the infected host spends its time and landscape_containers.R     -- Set up containers for model outputmosq_dispersal.R           -- Determine the expected number of host-seeking mosquitoes in a given landscape cellneeded_packages.R          -- Required packages to run this codeparameters_dynamic.R       -- For every given cell that the infected host enters determine which mosquitoes are there and where they come fromparameters_top.R           -- Set up the key parameters for the simulationparameters.R               -- Key transmission parameterspop_dens.R                 -- Obtain mosquito and other-host densities on the landscapeR0_calc.R                  -- Calculate host-to-mosquito transmission and mosquito-to-host transmission
results_containers.R       -- Set up containers for resultsrun_sim.R                  -- Run the WAIFW model across the landscape
sim_landscapes.R           -- Obtain simulated landscapes across a gradient of heterogeneitysummarize_results.R        -- Take calculated R0 and summarize for overall R0, direction of infection flow etc

----
Data
----

biting_rate.csv                 -- Some biting rate parameters from the lit for all relevant mosquito genera feeding_preference              -- Blood meal data extracted from the lit for all relevant mosquito genera flight_distance                 -- Some mark recapture and other data to help parameterize mosquito flight distancegonotrophic_cycle_length        -- For Haemagogus spp. to help figure out biting ratehaem_trans                      -- Transmission for Haemagogus spp. (separate file than mosquito_to_host_transmission because of a different style of experiment (generally))host_titer.csv                  -- Host physiological response to infectionhost_to_mosquito_transmission   -- Infection probability of mosquitoes given a bite on an infected host mosquito_to_host_transmission   -- Transmission probability of mosquitoes to hosts given a bite on a susceptible hostsurvival                        -- Mosquito survival 

-----------
Other files
-----------

bogota_samps.csv                                      -- CSV of NDVI for landscape near Bogota cali_samps.csv                                        -- CSV of NDVI for landscape near Calicor_-0.55_-0.45_mean_0.48_0.52_dim_60_sim_land.Rds    -- Saved simulated landscapesempirical_land_sim_land.Rds                           -- Saved simulated reforestation scenarios for "empirical" landscapes

