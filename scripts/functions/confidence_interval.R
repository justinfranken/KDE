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
  # set b
  b = h
  # estimates 
  estimates <- kde(x = x, eval = x_point, h = h, b = b, kernel = kernel)
  
  if (model ==  "bc"){
    
    ci_lower <- estimates$f_m - z * estimates$sd_f_k_hat  
    ci_upper <- estimates$f_m + z * estimates$sd_f_k_hat
  }
 
  if (model ==  "rbc"){
   
    ci_lower <- estimates$f_m - z * estimates$sd_f_m_hat  
    ci_upper <- estimates$f_m + z * estimates$sd_f_m_hat 
  }
  
  if (model ==  "us"){

    ci_lower <- estimates$f_k - z * estimates$sd_f_k_hat  
    ci_upper <- estimates$f_k + z * estimates$sd_f_k_hat 
  }

  ci <- c(ci_lower, ci_upper)
  
  return(ci)
}
