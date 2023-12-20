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
    
    # bias corrected density for x_point
    f_point_hat_bc<- kde_unbiased_univariate(x = x, eval = x_point, h = h, b = b, kernel = kernel)$f
    
    # estimation of standard deviation
    sd_point_hat_us <- kde_univariate(x = x, eval = x_point, h = h, kernel = kernel)$sd_f_hat
    
    # estimation of bias corrected confindence intervals
    ci_lower <- f_point_hat_bc - z * sd_point_hat_us  
    ci_upper <- f_point_hat_bc + z * sd_point_hat_us  
  }
 
  if (model ==  "rbc"){
   
    #auther recommendation
    b = h
    
    # bias corrected density for x_point
    f_point_hat_rbc <- kde_unbiased_univariate(x = x,eval = x_point, h = h, b = b, kernel = kernel)$f
    
    # estimation of standard deviation to get robust estimate
    sd_point_hat_rbc <- kde_unbiased_univariate(x= x,eval = x_point, h = h, b = b, kernel = kernel)$sd_f_hat

    # estimation of robust bias corrected confindence intervals
    ci_lower <- f_point_hat_rbc  - z * sd_point_hat_rbc 
    ci_upper <- f_point_hat_rbc  + z * sd_point_hat_rbc 
  }
  
  if (model ==  "us"){

    # density for x_point
    f_point_hat <- kde_univariate(x= x, eval = x_point, h = h, kernel = kernel)$f
    
    # estimation of standard deviation
    sd_point_hat_us <- kde_univariate(x = x, eval = x_point, h = h, kernel = kernel)$sd_f_hat

    # estimation of confindence intervals
    ci_lower <- f_point_hat - z * sd_point_hat_us 
    ci_upper <- f_point_hat + z * sd_point_hat_us 
  }

  ci <- c(ci_lower, ci_upper)
  
  return(ci)
}
