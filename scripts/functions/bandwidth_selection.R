################################################################################
#                                                                              #
# This script deploys functions for bandwidth selection.                       #
#                                                                              #
################################################################################

#### Rule of thumb methods

bandwidth_silverman <- function(x){
  ##############################################################################
  # Function for bandwidth selection as proposed by Silverman (1986, p.47).    #
  #                                                                            #
  # Args:                                                                      #
  #   x             Variable for which the bandwidth is calculated             #
  #                 (vector int/cnumeric)                                      #
  #                                                                            #
  # Returns:                                                                   #
  #   bw            Calculated bandwidth (numeric)                             #
  ##############################################################################
  bw <- 0.9 * min(sd(x), IQR(x)/1.35) * length(x)^(-1/5)
  
  return(bw)
}

bandwidth_scott <- function(x){
  ##############################################################################
  # Function for bandwidth selection as proposed by Scott (1992, p.152).       #
  #                                                                            #
  # Args:                                                                      #
  #   x             Variable for which the bandwidth is calculated             #
  #                 (vector int/cnumeric)                                      #
  #                                                                            #
  # Returns:                                                                   #
  #   bw            Calculated bandwidth (numeric)                             #
  ##############################################################################
  
  bw <- 1.06 * min(sd(x), IQR(x)/1.35) * length(x)^(-1/5)
  
  return(bw)
}

#### Plug in methods 

bandwidth_plug_in_sj <- bw.SJ

#### Least squares cross validation

bandwidth_cv <- bw.ucv

#### Hall - Undersmoothing

bandwidth_hall <- function(x,lambda = 0.1){
  ##############################################################################
  # Function for undersmoothed bandwidth selection as proposed by Hall (1992)  #
  #                                                                            #
  # Args:                                                                      #
  #   x             variable for which the bandwidth is calculated             #
  #                 (vector int/cnumeric)                                      #
  #   lambda        Adjust level of undersmoothing                             # 
  #                                                                            #
  #                                                                            #
  # Returns:                                                                   #
  #   bw            Calculated bandwidth (numeric)                             #
  ##############################################################################
  
  n <- length(x)
  bw <- lambda * n^(-1/5)
  
  return(bw)
}

