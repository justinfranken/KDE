################################################################################
#                                                                              #
# This script deploys functions for unbiased kde estimation                    #
#                                                                              #
################################################################################

# Load kernel functions
source(paste0(getwd(),"/scripts/functions/kernel.r"))

kde_unbiased_univariate <- function(
                           x,
                           eval = NULL,
                           h,
                           b,
                           kernel = "gaussian"
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
  if (is.null(eval)){
    # Calculation of n evenly distributed data points over the range of x
    eval <- seq(min(x),max(x),length.out = 30)}

  
  # Calculate the density for each x
  density    <- lapply(eval,
                 f_x_unbiased, 
                 x = x, 
                 h = h, 
                 b = b,
                 kernel_fun = kernel_fun,
                 kernel_fun_second_derivative = kernel_fun_second_derivative)
  
  density <- bind_rows(density)
  
  # f
  f <- density$f
  
  # Variance of f
  sd_f_hat <- sqrt(density$var_m_hat / (length(x)*h^2))
  
  # Store the results in a list
  out <- list(eval = eval, f = f, sd_f_hat = sd_f_hat)
  
  return(out)
}

f_x_unbiased <- function(eval,
                         x,
                         h,
                         b,
                         kernel_fun,
                         kernel_fun_second_derivative 
) {
  
  # Get n
  n <- length(x)
  
  # rho
  rho <- h/b
  
  # Calculate density with standard kernel k(.) using h
  v <- (x - eval)/h
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
  var_m_hat <- var(m_hat)
  
  return(list(f = f, var_m_hat = var_m_hat))
}





