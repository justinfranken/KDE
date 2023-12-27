# import libraries
source(paste0(getwd(),"/scripts/lib.r"))

# import sources
source(paste0(getwd(),"/scripts/functions/plots/coverage_prob_plot.r"))

# load simulation results
load(paste0(getwd(),"/data/simulations/coverage_prob_grid.Rda"))

#-------------------       coverage probability      ---------------------------

# Plot of the coverage probability for confidence interval construction 
# methods as a function of the sample size

coverage_prob_n_plot(data_model = "m1",
                     x_point = -2,
                     conf_int_model = c("bc","rbc"),
                     kernel = "epanechnikov"
)