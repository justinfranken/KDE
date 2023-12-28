################################################################################
#                                                                              #
# This script deploys functions for (unbiased) kde estimation                  #
#                                                                              #
################################################################################

kde <- function( x,
                 eval = NULL,
                 h,
                 b = NULL,
                 kernel = "gaussian",
                 ci = c("us","bc","rbc"),
                 alpha = 0.05
){
  
  #-----------------------------------------------------------------------------
  # Get kernel
  
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
  
  #-----------------------------------------------------------------------------
  # Select arguments for density function
  
  if (is.null(eval)){
    # Calculation of n evenly distributed data points over the range of x
    eval <- seq(min(x),max(x),length.out = 30)} 

  # By auther recommendation
  if (is.null(b)){
    b <- h
  }
  
  #-----------------------------------------------------------------------------
  # Calculate the density for each x
  
  density    <- lapply(eval,
                 f_x, 
                 x = x, 
                 h = h, 
                 b = b,
                 kernel_fun = kernel_fun,
                 kernel_fun_second_derivative = kernel_fun_second_derivative)
  
  density <- bind_rows(density)
  
  #-----------------------------------------------------------------------------
  # Extract important estimates
  
  # f_k
  f_k <- density$f_k
  # f_m
  f_m <- density$f_m
  
  # SD of f_k
  sd_f_k_hat <- sqrt(density$var_k_hat / (length(x)*h^2))
  
  # SD of f_m
  sd_f_m_hat <- sqrt(density$var_m_hat / (length(x)*h^2))
  
  # Store the results in a list
  out <- list(eval = eval, 
              f_k = f_k, 
              f_m = f_m,
              sd_f_k_hat = sd_f_k_hat,
              sd_f_m_hat = sd_f_m_hat)
  
  #-----------------------------------------------------------------------------
  # Calculate and append selected confidence intervals
  
  out$ci <- list()
  
  z <- qnorm(1-alpha/2, mean = 0, sd = 1)
  
  if ("bc" %in% ci){
    out$ci$bc <- c(f_m - z * sd_f_k_hat, f_m + z * sd_f_k_hat)
  }
  
  if ("rbc" %in% ci){
    out$ci$rbc <- c(f_m - z * sd_f_m_hat , f_m + z * sd_f_m_hat)
  }
  
  if ("us" %in% ci){
    out$ci$us <- c(f_k - z * sd_f_k_hat, f_k + z * sd_f_k_hat) 
  }
  
  return(out)
}

f_x <- function(eval,
                 x,
                 h,
                 b,
                 kernel_fun,
                 kernel_fun_second_derivative 
) {
  
  #-----------------------------------------------------------------------------
  # Standard Kernel k(.)

  # Get n
  n <- length(x)
  
  # Calculate density with standard kernel k(.) using h
  v <- (x - eval)/h
  k_hat <- kernel_fun(v)
  
  # Get density for x
  f_k <- sum(k_hat) / (length(x)*h)
  
  # Variance of kernel 
  var_k_hat <- mean(k_hat^2)-mean(k_hat)^2
  
  
  #-----------------------------------------------------------------------------
  # Bias corrected kernel m(.)
  
  # rho
  rho <- h/b
  
  # Calculate density with second derivative of l(.) using b
  l_2_hat <- kernel_fun_second_derivative(rho*v)
  
  # Get second moment of k(.)
  mu_k <- (1/2) * integrate(function(v) v^2 * kernel_fun(v), lower = -Inf, upper = Inf)$value 
  
  # Construct unbiased kernel m(.)
  m_hat <- k_hat - rho^3 * l_2_hat * mu_k 
  
  # Density f using m(.)
  f_m <- sum(m_hat) / (n*h)
  
  # Variance of kernel m(.)
  var_m_hat <- var(m_hat)
  
  return(list(f_k = f_k,
              f_m = f_m ,
              var_m_hat = var_m_hat,
              var_k_hat = var_k_hat))
}

#source(paste0(getwd(),"/scripts/functions/kernel.r"))
#source(paste0(getwd(),"/scripts/functions/kernel.r"))
#kde(x = rnorm(100000,0,1),eval = -4, h = 0.3, kernel ="epanechnikov")
