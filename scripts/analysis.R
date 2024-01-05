# import libraries
source(paste0(getwd(),"/scripts/lib.r"))

# import sources
source(paste0(getwd(),"/scripts/functions/plots/coverage_prob_plot.r"))
source(paste0(getwd(),"/scripts/functions/plots/interval_length_plot.r"))

# load simulation results
load(paste0(getwd(),"/data/simulations/coverage_prob_grid.Rda"))

#-------------------       coverage probability      ---------------------------

# Plot of the coverage probability for confidence interval construction 
# methods as a function of the sample size

# Plot for analysing an evaluation point
coverage_prob_n_plot(data = coverage_prob_grid,
                     data_model = "m1",
                     x_point = 0,
                     conf_int_model = c("bc","rbc","us"),
                     bandwidth_model = c("plug_in_sj","cv",
                                         "hall_0.3","hall_0.5","hall_0.7"),
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
                                bandwidth_model = c("hall_0.3","hall_0.5","hall_0.7"),
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
                                bandwidth_model = c("cv",
                                                    "hall_0.7"
                                                    ),
                                kernel = "epanechnikov"
)

#-------------------         interval length         ---------------------------

# Robust Bias correction with different bandwidth estimators
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("rbc"),
                                bandwidth_model = c("plug_in_sj", "cv","silverman","scott"),
                                kernel = "epanechnikov"
)
# -> One central observation
#      (1) Scott is superior

# undersmoothing 
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("us"),
                                bandwidth_model = c("hall_0.3","hall_0.5","hall_0.7"),
                                kernel = "epanechnikov"
)
# -> One central observation
#      (1) Hall_0.7 is superior
#          Explanation: a higher bandwidth leads to a lower variance


# Comparing best combinations
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("rbc","us"),
                                bandwidth_model = c("hall_0.7","scott"),
                                kernel = "epanechnikov"
)

# -> One central observation
#      (1) Undersmoothing is better in terms of inter length
#          Explanation: RBC Standard Errors > US Standard Error, because
#          RBC Standard Error carry variance of Bias: 
#          Var(bias corrected estimate) = Var(f-Bias) = Var(f) + Var(Bias) - 2*Cov(f,Bias)
