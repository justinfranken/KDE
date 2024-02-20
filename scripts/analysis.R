#--------------------     Monte-Carlo Simulation          ----------------------

# import libraries
source(paste0(getwd(),"/scripts/lib.r"))

# import sources
source(paste0(getwd(),"/scripts/functions/plots/coverage_prob_plot.r"))
source(paste0(getwd(),"/scripts/functions/plots/interval_length_plot.r"))

# load simulation results
load(paste0(getwd(),"/data/simulations/coverage_prob_grid.Rda"))

# choose data model
data_model = "m1"

#-------------------     5.1 Effect on coverage probability        -------------

#--------------   US
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = data_model,
                                conf_int_model = c("us"),
                                bandwidth_model = c("silverman"),
                                lambda =  c(0.6,0.7,0.8,0.9,1),
                                eta = 1,
                                kernel = "epanechnikov")

#--------------   RBC and BC
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = data_model,
                                conf_int_model = c("rbc","bc"),
                                bandwidth_model = c("plug_in_sj", "cv","silverman"),
                                lambda = 1,
                                eta = 1,
                                kernel = "epanechnikov"
) 

#--------------   Best models
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = data_model,
                                conf_int_model = c("rbc","us","bc"),
                                bandwidth_model = c("silverman"),
                                lambda = 1,
                                eta = 1,
                                kernel = "epanechnikov"
)

#-------------------     5.2 Effect on interval length     ---------------------

#--------------   US
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = data_model,
                                conf_int_model = c("us"),
                                bandwidth_model = c("silverman"),
                                lambda = c(0.6,0.7,0.8,0.9,1),
                                eta = 1,
                                kernel = "epanechnikov")

#--------------   RBC and BC 
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                  data_model = data_model,
                                  conf_int_model = c("rbc","bc"),
                                  bandwidth_model = c("plug_in_sj", "cv","silverman"),
                                  lambda = 1,
                                  eta = 1,
                                  kernel = "epanechnikov")

#--------------   Best models
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = data_model,
                                conf_int_model = c("rbc","us","bc"),
                                bandwidth_model = c("silverman"),
                                lambda = 1,
                                eta = 1,
                                kernel = "epanechnikov"
)

#-------------------     5.3 Relaxing b=h      ---------------------------------

#--------------   RBC
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                  data_model = data_model,
                                  conf_int_model = c("rbc"),
                                  bandwidth_model = c("silverman"),
                                  lambda = 1,
                                  eta = c(0.6,0.7,0.8,0.9,1),
                                  kernel = "epanechnikov"
)

#--------------   RBC
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = data_model,
                                conf_int_model = c("rbc"),
                                bandwidth_model = c("silverman"),
                                lambda = 1,
                                eta = c(0.6,0.7,0.8,0.9,1),
                                kernel = "epanechnikov"
)


