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
  density    <- lapply(x,
                 f_x_unbiased, 
                 x_train = x_train, 
                 h = h, 
                 b = b,
                 kernel_fun = kernel_fun,
                 kernel_fun_second_derivative = kernel_fun_second_derivative)
  
  density <- bind_rows(density)
  
  # f
  f <- density$f
  
  # Variance of f
  var_f_hat <- density$var_m_hat / (length(x_train)*h^2)
  
  # Store the results in a list
  out <- list(x = x, f = f, var_f_hat = var_f_hat)
  
  return(out)
}

f_x_unbiased <- function(x,
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
  
  # Construct unbiased kernel m(.)
  m_hat <- k_hat - rho^3 * l_2_hat * mu_k 
  
  # Density f using m(.)
  f <- sum(m_hat) / (n*h)
  
  # Variance of kernel m(.)
  var_m_hat <- mean(m_hat^2)-mean(m_hat)^2
  
  return(list(f = f, var_m_hat = var_m_hat))
}


#set.seed(4322)
#kde <- kde_unbiased_univariate(x_train= rnorm(100, mean = 4),h = 0.3,b=0.3, kernel = "gaussian", n = 30)
#plot(x = kde$x, y = kde$y,type ="l")


