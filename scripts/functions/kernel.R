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
  y <- (3/4)*(1-v^2) * ifelse(abs(v)<=1,1,0)
  return(y)
}

epanechnikov_kernel_second_derivative <- function(v){
  y <- (3/2) * ifelse(abs(v)<=1,1,0)
  return(y)
}