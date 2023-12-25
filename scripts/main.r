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

S <- 10000
alpha <- 0.05

#-------------------       coverage probability      ---------------------------

data_model <- c("m1")
x_point <- c(4,2,0,-2,-4)
n <-  seq(10,100,10)
bandwidth_model <- c("cv")
conf_int_model <- c("bc","rbc")
kernel = c("epanechnikov")

# Create a data frame with all combinations
coverage_prob_grid <- expand.grid(data_model = data_model,
                                  x_point = x_point,
                                  n = n,
                                  bandwidth_model = bandwidth_model,
                                  conf_int_model = conf_int_model,
                                  kernel = kernel)
  
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
  
}


