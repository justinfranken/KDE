# import libraries
source(paste0(getwd(),"/scripts/lib.r"))

# import sources
source(paste0(getwd(),"/scripts/functions/plots/coverage_prob_plot.r"))

# load simulation results
load(paste0(getwd(),"/data/simulations/coverage_prob_grid.Rda"))

#-------------------       coverage probability      ---------------------------

# Plot of the coverage probability for confidence interval construction 
# methods as a function of the sample size

# Plot for analysing an evaluation point
coverage_prob_n_plot(data = coverage_prob_grid,
                     data_model = "m1",
                     x_point = 0,
                     conf_int_model = c("us","bc","rbc"),
                     bandwidth_model = c("plug_in_sj","cv",
                                         "hall_0.3","hall_0.5","hall_0.7",
                                         "plug_in_sj_0.3","plug_in_sj_0.5","plug_in_sj_0.7"
                                         ),
                     kernel = "epanechnikov",
                     x_axis_log = FALSE
)

# Robust Bias correction with different bandwidth estimators
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("rbc"),
                                bandwidth_model = c("plug_in_sj", "cv","silverman","scott"),
                                kernel = "epanechnikov"
)
# -> Three observations
#      (1) Evaluation point effects coverage probability
#      (2) Coverage probability at 95% for higher n
#      (3) Bandwidth estimators play a minor role (for coverage probability, 
#          but probably for interval length)

# Undersmoothing with different bandwidth estimators
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("us"),
                                bandwidth_model = c("hall_0.3","hall_0.5","hall_0.7",
                                                    "plug_in_sj_0.3","plug_in_sj_0.5","plug_in_sj_0.7"),
                                kernel = "epanechnikov"
)
# -> Two central observation
#      (1) Evaluation point effects coverage probability
#      (2) Coverage probability at 95% for higher n
#      (3) Bandwidth estimators play a larger role 

# Bias correction with different bandwidth estimators
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("bc"),
                                bandwidth_model = c("plug_in_sj", "cv","silverman","scott"),
                                kernel = "epanechnikov"
)
# -> One central observation
#      (1) We have no valid construction of confidence interval's,
#          since rho does not go to zero


# Comparing best combinations
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("bc","rbc","us"),
                                bandwidth_model = c("plug_in_sj",
                                                    "cv",
                                                    "plug_in_sj_0.3",
                                                    "plug_in_sj_0.5",
                                                    "plug_in_sj_0.7"),
                                kernel = "epanechnikov"
)

#-------------------         interval length         ---------------------------


