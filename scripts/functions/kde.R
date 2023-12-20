################################################################################
#                                                                              #
# This script deploys functions for Kernel Density Estimation.                 #
#                                                                              #
################################################################################

# Load kernel functions
source(paste0(getwd(),"/scripts/functions/kernel.r"))

kde_univariate <- function(x,
                           eval,
                           h,
                           kernel = "gaussian"
                           ){
  
  # Check if kernel is available
  if(!kernel %in% c("gaussian","epanechnikov")){
    stop("Selected kernel function is not available")
  }
  
  # Select kernel
  if (kernel == "gaussian"){
    kernel_fun = gaussian_kernel
  }
  if (kernel == "epanechnikov"){
    kernel_fun = epanechnikov_kernel
  }
  
  # Select arguments for density function
  if (is.null(eval)){
    # Calculation of n evenly distributed data points over the range of x_obs
    eval <- seq(min(x),max(x),length.out = 30)
  }
  
  # Calculate the density for each x
  density    <- lapply(eval,
                       kde_get_prob_x, 
                       x = x, 
                       h = h, 
                       kernel_fun = kernel_fun)
  
  density <- bind_rows(density)
  
  # f
  f <- density$f
  
  # Variance of f
  sqrt(sd_f_hat <- density$var_k_hat / (length(x)*h^2))

    # Store the results in a list
  kde_out <- list(eval = eval, f = f, sd_f_hat = sd_f_hat)
  
  return(kde_out)
}

kde_get_prob_x <- function(eval,
                           x,
                           h,
                           kernel_fun
                           ) {
  
  # Apply kernel function
  k_hat <- kernel_fun((x - eval)/h)
  
  # Get density for x
  f <- sum(k_hat) / (length(x)*h)
  
  # Variance of kernel 
  var_k_hat <- mean(k_hat^2)-mean(k_hat)^2
  
  # Return density
  return(list(f = f, var_k_hat = var_k_hat))
}

# test functions
#set.seed(4322)
#kde <- kde_univariate(x_train= rnorm(100, mean = 4), h = 0.3, kernel = "gaussian", density_for_x = 0)
#plot(x = kde$x, y = kde$f,type ="l")

