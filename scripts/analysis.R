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

#--------------   RBC
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("rbc"),
                                bandwidth_model = c("plug_in_sj", "cv","silverman","scott"),
                                kernel = "epanechnikov"
)

# -> General observations independent of confidence model
#      (1) Evaluation point effects coverage probability
#      (2) Coverage probability depends on n 

# -> One main observation
#      (1) Bandwidth estimators play a minor role (for coverage probability, 
#          but probably for interval length)
#            

#--------------   US
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("us"),
                                bandwidth_model = c("hall_0.3","hall_0.5","hall_0.7"),
                                kernel = "epanechnikov"
)
# -> One main observation
#      (1) Bandwidth estimators play a larger role 
#             - Lowering lambda can reduce bias but increases the variance of f_k
#               especially if n is small
#      (2) General Problem: How do choose lambda in practice? Since true distribution
#          is not available?
#             - For Model 1-4, we see a faster convergence for lambda = 0.7

#--------------   BC 
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("bc"),
                                bandwidth_model = c("plug_in_sj", "cv","silverman","scott"),
                                kernel = "epanechnikov"
)

# -> One main observation
#      (1) We have no valid construction of confidence interval's.
#          Reason: Since rho does not go to zero, we don't account for the variance
#          of the bias 

#--------------   Best models
coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("bc","rbc","us"),
                                bandwidth_model = c("cv",
                                                    "hall_0.7"
                                                    ),
                                kernel = "epanechnikov"
)

# -> One main observation
#      (1) Convergence of rbc seems to be faster in comparison to us

#-------------------         interval length         ---------------------------

#--------------   RBC
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("rbc"),
                                bandwidth_model = c("plug_in_sj", "cv","silverman","scott"),
                                kernel = "epanechnikov"
)
# -> One cmain observation
#      (1) Scott is superior for Model 1-4

#--------------   US
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("us"),
                                bandwidth_model = c("hall_0.3","hall_0.5","hall_0.7"),
                                kernel = "epanechnikov"
)
# -> One main observation
#      (1) Hall_0.7 is superior for Model 1-4
#          Explanation: a higher bandwidth leads to a lower variance


#--------------   Best models
interval_length_n_plot_all_points(data = coverage_prob_grid,
                                data_model = "m1",
                                conf_int_model = c("rbc","us"),
                                bandwidth_model = c("hall_0.7","scott"),
                                kernel = "epanechnikov"
)

# -> One central observation
#      (1) Undersmoothing is better in terms of interval length.
#          Explanation: RBC Standard Error carry variance of Bias: 
#          Var(bias corrected estimate) = Var(f-Bias) = Var(f) + Var(Bias) - 2*Cov(f,Bias)
