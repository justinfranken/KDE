################################################################################
#                                                                              #
# This script deploys functions for bandwidth selection.                       #
#                                                                              #
################################################################################

#### Rule of thumb methods

bandwidth_silverman <- function(x){
  ##############################################################################
  # Function for bandwidth selection as proposed by Silverman                  #
  #                                                                            #
  # Args:                                                                      #
  #   x             Variable for which the bandwidth is calculated             #
  #                 (vector int/numeric)                                      #
  #                                                                            #
  # Returns:                                                                   #
  #   bw            Calculated bandwidth (numeric)                             #
  ##############################################################################
  
  bw <- 1.06 * length(x)^(-1/5)
  
  return(bw)
}

#### Plug in methods 

bandwidth_plug_in_sj <- bw.SJ

#### Least squares cross validation

bandwidth_cv <- bw.ucv
