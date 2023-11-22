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

#bandwidth_plug_in <- function(x,kernel_fun,h_pilot){
#  ...
#}

#### Least squares cross validation

#bandwidth_plug_in <- function(x,kernel_fun{
#  ...
#}