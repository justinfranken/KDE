# simulate six distributions based on Marron and Wand (1992)

conf_int <- function(x,
                     x_point,
                     h,
                     kernel,
                     alpha,
                     model
){
  
  if (model ==  "bc"){
  
    #auther recommendation
    b = h
    
    # density for x_point
    f_hat_of_x <- kde_univariate(x_train = x, h = h, kernel = kernel, density_for_x = x_point)$y
    
    # bias for x_point
    bias_point <- bias_univariate(x_train = x, h = h, b = b, kernel = kernel, bias_for_x = x_point)$y
    
    # bias corrected estimate for x_point
    f_hat_of_x_bc <- f_hat_of_x - bias_point
    
    # estimation of standard deviation
    f_hat <- kde_univariate(x_train = x, h = h, kernel = kernel)$y
    sd_hat <- sd(f_hat)
    
    # estimation of bias corrected confindence intervals
    z <- qnorm(1-alpha/2, mean = 0, sd = 1)
    ci_lower <- f_hat_of_x_bc - z * (sd_hat / sqrt(n*h))
    ci_upper <- f_hat_of_x_bc + z * (sd_hat / sqrt(n*h))
    ci <- c(ci_lower, ci_upper)
  }
 
  if (model ==  "rbc"){
   
    #auther recommendation
    b = h
    
    # density for x_point
    f_hat_of_x <- kde_univariate(x_train = x, h = h, kernel = kernel, density_for_x = x_point)$y
    
    # bias for x_point
    bias_point <- bias_univariate(x_train = x, h = h, b = b, kernel = kernel, bias_for_x = x_point)$y
    
    # bias corrected estimate for x_point
    f_hat_of_x_rbc <- f_hat_of_x - bias_point
    
    # estimation of standard deviation to get robust estimate
    f_hat <- kde_univariate(x_train = x, h = h, kernel = kernel)$y
    bias <- bias_univariate(x_train = x, h = h, b = b, kernel = kernel)$y
    sd_hat <- sd(f_hat-bias)
    
    # estimation of robust bias corrected confindence intervals
    z <- qnorm(1-alpha/2, mean = 0, sd = 1)
    ci_lower <- f_hat_of_x_rbc - z * (sd_hat / sqrt(n*h))
    ci_upper <- f_hat_of_x_rbc + z * (sd_hat / sqrt(n*h))
    ci <- c(ci_lower, ci_upper)
  }
  
  if (model ==  "us"){

    # density for x_point
    f_hat_of_x <- kde_univariate(x_train = x, h = h, kernel = kernel, density_for_x = x_point)$y
    
    # estimation of standard deviation
    f_hat <- kde_univariate(x_train = x, h = h, kernel = kernel)$y
    sd_hat <- sd(f_hat)
    
    # estimation of confindence intervals
    z <- qnorm(1-alpha/2, mean = 0, sd = 1)
    ci_lower <- f_hat_of_x - z * (sd_hat / sqrt(n*h))
    ci_upper <- f_hat_of_x + z * (sd_hat / sqrt(n*h))
    ci <- c(ci_lower, ci_upper)
  }
  
  #return bias corrected confidence interval
  return(ci)
}
