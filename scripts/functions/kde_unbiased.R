################################################################################
#                                                                              #
# This script deploys functions for unbiased kde estimation                    #
#                                                                              #
################################################################################

# Load kernel functions
source(paste0(getwd(),"/scripts/functions/kernel.r"))

kde_unbiased_univariate <- function(
                           x_train,
                           h,
                           b,
                           kernel = "gaussian",
                           n = 100,
                           density_for_x = NA
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
  if (is.na(density_for_x)){
    # Calculation of n evenly distributed data points over the range of x_obs
    x <- seq(min(x_train),max(x_train),length.out = n)
  }else{
    x <- density_for_x
  }
  
  # Calculate the density for each x
  y    <- sapply(x,
                 f_x_unbiased, 
                 x_train = x_train, 
                 h = h, 
                 b = b,
                 kernel_fun = kernel_fun,
                 kernel_fun_second_derivative = kernel_fun_second_derivative)
  
  # Store the results in a list
  bias_out <- list(x = x, y = y)
  
  return(bias_out)
}

f_x_unbiased <- unbiased <- function(x,
                           x_train,
                           h,
                           b,
                           kernel_fun,
                           kernel_fun_second_derivative 
) {
  
  # Get n
  n <- length(x_train)
  
  # rho
  rho <- h/b
  
  # Calculate density with standard kernel k(.) using h
  v <- (x_train - x)/h
  k_hat <- kernel_fun(v)
  
  # Calculate density with second derivative of l(.) using b
  l_2_hat <- kernel_fun_second_derivative(rho*v)
  
  # Get second moment of k(.)
  mu_k <- (1/2) * integrate(function(v) v^2 * kernel_fun(v), lower = -Inf, upper = Inf)$value 
  
  m_hat <- k_hat - rho^3 * l_2_hat * mu_k 
  
  # Return density
  f <- sum(m_hat) / (n*h)
  return(f)
}


#set.seed(4322)
#kde <- kde_unbiased_univariate(x_train= rnorm(100, mean = 4),h = 0.3,b=0.3, kernel = "gaussian", n = 30)
#plot(x = kde$x, y = kde$y,type ="l")


