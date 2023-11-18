################################################################################
#                                                                              #
# This script deploys functions for bandwidth selection.                       #
#                                                                              #
################################################################################

# silverman
bandwidth_silverman <- function(x){
  
  bw <- 0.9 * min(sd(x), IQR(x)/1.35) * length(x)^(-1/5)
  
  return(bw)
}

# scott
bandwidth_scott <- function(x){

  bw <- 1.06 * min(sd(x), IQR(x)/1.35) * length(x)^(-1/5)
  
  return(bw)
}
