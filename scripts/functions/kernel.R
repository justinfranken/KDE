################################################################################
#                                                                              #
# This script contains kernel functions                                        #
#                                                                              #
################################################################################

gaussian_kernel <- function(v){
  
  y <- (1/sqrt(2*pi)) * exp(-(1/2)*v^2)
  return(y)
}

gaussian_kernel_second_derivative <- function(v){
  
  y <- (1/sqrt(2*pi))  * exp(-(1/2)*v^2) * (v^2 - 1)
  return(y)
}

epanechnikov_kernel <- function(v){
  y <- (abs(v)<=1) * (3/4)*(1-v^2) 
  return(y)
}

epanechnikov_kernel_second_derivative <- function(v){
  y <- (abs(v)<=1)*15*(3*v^2-1)/4
  return(y)
}