# import libraries
source(paste0(getwd(),"/scripts/lib.r"))

# import sources
source(paste0(getwd(),"/scripts/functions/kernel.r"))
source(paste0(getwd(),"/scripts/functions/bandwidth_selection.r"))
source(paste0(getwd(),"/scripts/functions/kde.r"))
source(paste0(getwd(),"/scripts/functions/coverage_prob.r"))

# set seed for reproducibility
set.seed(4322)

#-------------------         fix parameters          ---------------------------

S <- 1000
alpha <- 0.05

#-------------------       coverage probability      ---------------------------

# Basis for construction of combinations
data_model = c("m1")
x_point <- c(4,2,0,-2,-4)
#n <-  c(seq(25,500,25), seq(750, 5000, 250), seq(5000, 10000, 1000))
n <- seq(25,500,25)
kernel = c("epanechnikov")

# Create a data frame with all combinations
coverage_prob_grid_1 <- expand.grid(data_model = data_model,
                                    x_point = x_point,
                                    n = n,
                                    bandwidth_model = c("cv","plug_in_sj"),
                                    conf_int_model = c("bc","rbc"),
                                    kernel = kernel)

coverage_prob_grid_2 <- expand.grid(data_model = data_model,
                                    x_point = x_point,
                                    n = n,
                                    bandwidth_model = c("hall_0.3","hall_0.5","hall_0.7",
                                                        "plug_in_sj_0.3","plug_in_sj_0.5","plug_in_sj_0.7"),
                                    conf_int_model = c("us"),
                                    kernel = kernel)

coverage_prob_grid <- rbind(coverage_prob_grid_1,
                            coverage_prob_grid_2)

# Esimtate coverage probability for all combinations
for (i in c(1:nrow(coverage_prob_grid))){
  
  param <- coverage_prob_grid[i,]
  
  coverage_prob <- coverage_for_n(
                       n = param$n,
                       S = S, 
                       data_model = param$data_model,
                       x_point = param$x_point, 
                       bandwidth_model = param$bandwidth_model ,
                       conf_int_model = param$conf_int_model,
                       kernel = param$kernel,
                       alpha = alpha)
  
  coverage_prob_grid[i,"coverage_prob"] <- coverage_prob
  
  print(coverage_prob_grid[i,])
  
}

# Save simulation results
save(coverage_prob_grid, file="data/simulations/coverage_prob_grid.Rda")

