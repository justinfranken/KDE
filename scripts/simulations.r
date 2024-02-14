# import libraries
source(paste0(getwd(),"/scripts/lib.r"))

# import sources
source(paste0(getwd(),"/scripts/functions/kernel.r"))
source(paste0(getwd(),"/scripts/functions/bandwidth_selection.r"))
source(paste0(getwd(),"/scripts/functions/kde.r"))
source(paste0(getwd(),"/scripts/functions/data_generator.r"))
source(paste0(getwd(),"/scripts/functions/coverage_prob.r"))

# set seed for reproducibility
set.seed(4322)

#-------------------         fix parameters          ---------------------------

S <- 5000
alpha <- 0.05

#-----        Analysis of coverage probability and interval length          ----

# Basis for construction of combinations
data_model <- c("m1","m2","m3","m4")
x_point <- seq(-2,2,1)
n <- seq(25,500,25)
kernel <- c("epanechnikov")

# Create a data frame with all combinations

# grid for bias correction
coverage_prob_grid_1 <- expand.grid(data_model = data_model,
                                    x_point = x_point,
                                    n = n,
                                    bandwidth_model = c("cv","plug_in_sj","silverman"),
                                    eta = 1, # b = h
                                    lambda = 1, # no undersmoothing 
                                    conf_int_model = c("bc"),
                                    kernel = kernel)
# grid for robust bias correction
coverage_prob_grid_2 <- expand.grid(data_model = data_model,
                                    x_point = x_point,
                                    n = n,
                                    bandwidth_model = c("cv","plug_in_sj","silverman"),
                                    eta = c(0.2,0.4,0.6,0.8,1), 
                                    lambda = 1, # no undersmoothing 
                                    conf_int_model = c("rbc"),
                                    kernel = kernel)
# grid for undersmoothing
coverage_prob_grid_3 <- expand.grid(data_model = data_model,
                                    x_point = x_point,
                                    n = n,
                                    bandwidth_model = "silverman",
                                    eta = 1, # dummy placeholder
                                    lambda = c(0.2,0.4,0.6,0.8,1), # undersmoothing
                                    conf_int_model = c("us"),
                                    kernel = kernel)

# rowbind grids
coverage_prob_grid <- rbind(coverage_prob_grid_1,
                            coverage_prob_grid_2,
                            coverage_prob_grid_3)

grid_length <- nrow(coverage_prob_grid)

tic()

# Estimate coverage probability for all combinations
for (i in c(1:grid_length)){
    
  param <- coverage_prob_grid[i,]
  
  coverage <- coverage_for_n(
                   n = as.integer(param$n),
                   S = as.integer(S), 
                   data_model = as.character(param$data_model),
                   x_point = as.integer(param$x_point), 
                   bandwidth_model = as.character(param$bandwidth_model),
                   eta = as.numeric(param$eta),
                   lambda = as.numeric(param$lambda),
                   conf_int_model = as.character(param$conf_int_model),
                   kernel = as.character(param$kernel),
                   alpha = as.numeric(alpha)
                   )
  
  coverage_prob_grid[i,"coverage_prob"] <- coverage$coverage_prob
  coverage_prob_grid[i,"ci_lower"] <- coverage$ci_lower
  coverage_prob_grid[i,"ci_upper"] <- coverage$ci_upper
  
  
  print(paste0(i," / ", grid_length))
  print(coverage_prob_grid[i,])
  
}
toc()

# Save simulation results
#save(coverage_prob_grid, file="data/simulations/coverage_prob_grid.Rda")