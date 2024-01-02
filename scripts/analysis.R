# import libraries
source(paste0(getwd(),"/scripts/lib.r"))

# import sources
source(paste0(getwd(),"/scripts/functions/plots/coverage_prob_plot.r"))

# load simulation results
#load(paste0(getwd(),"/data/simulations/coverage_prob_grid.Rda"))

#-------------------       coverage probability      ---------------------------

# Plot of the coverage probability for confidence interval construction 
# methods as a function of the sample size

coverage_prob_n_plot(data = coverage_prob_grid,
                     data_model = "m1",
                     x_point = 0,
                     conf_int_model = c("us","bc","rbc"),
                     bandwidth_model = c("cv","plug_in_sj",
                                         "hall_0.3","hall_0.5","hall_0.7",
                                         "plug_in_sj_0.3","plug_in_sj_0.5","plug_in_sj_0.7"
                                         ),
                     kernel = "epanechnikov",
                     x_axis_log = FALSE
)

coverage_prob_n_plot_all_points(data = coverage_prob_grid,
                                 data_model = "m1",
                                 conf_int_model = c("bc","rbc","us"),
                                 bandwidth_model = c("plug_in_sj", "plug_in_sj_0.7"),
                                 kernel = "epanechnikov"
                                 )


