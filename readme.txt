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

adj_models.R               -- Predictions from fitted models to physiological responses of hosts and mosquitoes
fit_models.R               -- Fit models to physiological responses of hosts and mosquitoes
results_containers.R       -- Set up containers for results
sim_landscapes.R           -- Obtain simulated landscapes across a gradient of heterogeneity

----
Data
----

biting_rate.csv                 -- Some biting rate parameters from the lit for all relevant mosquito genera 

-----------
Other files
-----------

bogota_samps.csv                                      -- CSV of NDVI for landscape near Bogota 
