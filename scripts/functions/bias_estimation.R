################################################################################
#                                                                              #
# This script deploys functions for Bias Estimation.                           #
#                                                                              #
################################################################################

# Load kernel functions
source(paste0(getwd(),"/scripts/functions/kernel.r"))

bias_univariate <- function(x_train,
                           h,
                           b,
                           kernel = "gaussian",
                           n = 100,
                           bias_for_x = NA
){
  
  # Check if kernel is available
  if(!kernel %in% c("gaussian","epanechnikov")){
    stop("Selected kernel function is not available")
  }
  
  # Select kernel
  if (kernel == "gaussian"){
    kernel_fun = gaussian_kernel
    kernel_fun_second_derivative = gaussian_kernel_second_derivative
    
  }
  if (kernel == "epanechnikov"){
    kernel_fun = epanechnikov_kernel
    kernel_fun_second_derivative = epanechnikov_kernel_second_derivative
    
  }
  
  # Select arguments for density function
  if (is.na(bias_for_x)){
    # Calculation of n evenly distributed data points over the range of x_obs
    x <- seq(min(x_train),max(x_train),length.out = n)
  }else{
    x <- bias_for_x
  }
  
  # Calculate the density for each x
  y    <- sapply(x,
                 get_bias_for_x, 
                 x_train = x_train, 
                 h = h, 
                 b = b,
                 kernel_fun = kernel_fun,
                 kernel_fun_second_derivative = kernel_fun_second_derivative)
  
  # Store the results in a list
  bias_out <- list(x = x, y = y)
  
  return(bias_out)
}

get_bias_for_x <- function(x,
                           x_train,
                           h,
                           b,
                           kernel_fun,
                           kernel_fun_second_derivative 
) {
  
  # Get argument for kernel 
  v <- (x_train - x)/b
  
  # Apply second derivative of kernel function
  vec_f_second_derivative <- kernel_fun_second_derivative(v)
  
  # Get n
  n <- length(x_train)
  
  # Get second derivative of f(x)
  f_second_derivative <- sum(vec_f_second_derivative) / (n * (b^3)) 
  
  # Get second moment of k(v)
  k_second_moment <- integrate(function(v) v^2 * kernel_fun(v), lower = -Inf, upper = Inf)$value 
  
  bias <- (h^2) * f_second_derivative * k_second_moment /2
  
  # Return density
  return(bias)
}




