# import libraries
source(paste0(getwd(),"/scripts/lib.r"))

# import sources
source(paste0(getwd(),"/scripts/functions/kernel.r"))
source(paste0(getwd(),"/scripts/functions/bandwidth_selection.r"))
source(paste0(getwd(),"/scripts/functions/kde.r"))
source(paste0(getwd(),"/scripts/functions/coverage_prob.r"))

# set seed for reproducibility
set.seed(4322)

#-------------------       coverage probability      ---------------------------

n_range <- c(10,20,30,40,50,100,1000,10000)

coverage_vec <- c()
  
for (n in n_range){
  coverage <- coverage_for_n(
                 n = n,
                 S = 500, # Simulations
                 data_model = "m1",
                 x_point = 0, # evaluation point for ci
                 bandwidth_model = "cv" ,
                 conf_int_model = "rbc",
                 kernel = "gaussian",
                 alpha = 0.05)
  
  coverage_vec <- c(coverage_vec,coverage)
  
}




