# simulate six distributions based on Marron and Wand (1992)

conf_int <- function(x,
                     x_point,
                     h,
                     kernel,
                     alpha,
                     model
){
  
  # n
  n <- length(x)
  # z
  z <- qnorm(1-alpha/2, mean = 0, sd = 1)
  
  if (model ==  "bc"){
    
    #auther recommendation
    b = h
    
    # density for x_point
    f_point_hat <- kde_univariate(x_train = x, h = h, kernel = kernel, density_for_x = x_point)$f
    
    # bias corrected density for x_point
    f_point_hat_bc<- kde_unbiased_univariate(x_train = x, h = h, b = b, kernel = kernel, density_for_x = x_point)$f
    
    # estimation of standard deviation
    var_point_hat_us <- kde_univariate(x_train = x, h = h, kernel = kernel)$var_f_hat
    sd_point_hat_us <- sqrt(var_point_hat_us)
    
    # estimation of bias corrected confindence intervals
    ci_lower <- f_point_hat_bc - z * (sd_point_hat_us  / sqrt(n*h))
    ci_upper <- f_point_hat_bc + z * (sd_point_hat_us  / sqrt(n*h))
  }
 
  if (model ==  "rbc"){
   
    #auther recommendation
    b = h
    
    # density for x_point
    f_point_hat <- kde_univariate(x_train = x, h = h, kernel = kernel, density_for_x = x_point)$f
    
    # bias corrected density for x_point
    f_point_hat_rbc <- kde_unbiased_univariate(x_train = x, h = h, b = b, kernel = kernel, density_for_x = x_point)$f
    
    # estimation of standard deviation to get robust estimate
    var_point_hat_rbc <- kde_unbiased_univariate(x_train = x, h = h, b = b, kernel = kernel)$var_f_hat
    sd_point_hat_rbc <- sqrt(var_point_hat_rbc)
    
    # estimation of robust bias corrected confindence intervals
    ci_lower <- f_point_hat_rbc  - z * (sd_point_hat_rbc / sqrt(n*h))
    ci_upper <- f_point_hat_rbc  + z * (sd_point_hat_rbc / sqrt(n*h))
  }
  
  if (model ==  "us"){

    # density for x_point
    f_point_hat <- kde_univariate(x_train = x, h = h, kernel = kernel, density_for_x = x_point)$f
    
    # estimation of standard deviation
    var_point_hat_us <- kde_univariate(x_train = x, h = h, kernel = kernel, density_for_x = x_point)$var_f_hat
    sd_point_hat_us <- sqrt(var_point_hat_us)
    
    # estimation of confindence intervals
    ci_lower <- f_point_hat - z * (sd_point_hat_us / sqrt(n*h))
    ci_upper <- f_point_hat + z * (sd_point_hat_us / sqrt(n*h))
  }

  ci <- c(ci_lower, ci_upper)
  
  return(ci)
}
