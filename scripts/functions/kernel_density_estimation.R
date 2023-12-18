################################################################################
#                                                                              #
# This script deploys functions for Kernel Density Estimation.                 #
#                                                                              #
################################################################################

# Load kernel functions
source(paste0(getwd(),"/scripts/functions/kernel.r"))

kde_univariate <- function(x_train,
                           h,
                           kernel = "gaussian",
                           n = 100,
                           density_for_x = NA
                           ){
  ##############################################################################
  # Function for Univariate Kernel Density Estimation.                         #
  #                                                                            #
  # Args:                                                                      #
  #   x_train       Continuous variable which is used to calculate the         #
  #                 density (int/numeric vector)                               #
  #   h             Bandwidth parameter (int/numeric)                          #         
  #   kernel        Kernel function (string): "gaussian" or "epanechnikov"     #
  #   n             Number of data for which the density is calculated (int)   #
  #                                                                            #
  # Returns:                                                                   #
  #   kde_out       List containing the continuous x variable with calculated  #
  #                 densities (y)                                              #
  ##############################################################################
  
  # Check if kernel is available
  if(!kernel %in% c("gaussian","epanechnikov")){
    stop("Selected kernel function is not available")
  }
  
  # Select kernel
  if (kernel == "gaussian"){
    kernel_fun = gaussian_kernel
  }
  if (kernel == "epanechnikov"){
    kernel_fun = epanechnikov_kernel
  }
  
  # Select arguments for density function
  if (is.na(density_for_x)){
    # Calculation of n evenly distributed data points over the range of x_obs
    x <- seq(min(x_train),max(x_train),length.out = n)
  }else{
    x <- density_for_x
  }
  
  # Calculate the density for each x
  y    <- sapply(x,
                 kde_get_prob_x, 
                 X_train = x_train, 
                 h = h, 
                 kernel_fun = kernel_fun)
  
  # Store the results in a list
  kde_out <- list(x = x, y = y)
  
  return(kde_out)
}

kde_get_prob_x <- function(x,
                           X_train,
                           h,
                           kernel_fun
                           ) {
  ##############################################################################
  # Function which maps each x to its density.                                 #
  #                                                                            #
  # Args:                                                                      #
  #   x             x for which the density is calculated (int/numer)          #          
  #   X_train       Observed training variables (int/numeric vector)           #
  #   h             Bandwidth parameter (int/numeric)                          #         
  #   kernel_fun    Kernel function.                                           #      
  #                                                                            #
  # Returns:                                                                   #
  #   f             Density of x (int/numeric)                                 #
  ##############################################################################
  
  # Apply kernel function
  vec_f <- kernel_fun((X_train - x)/h)
  
  # Get density for x
  f <- sum(vec_f) / (length(X_train)*h)
  
  # Return density
  return(f)
}

# test functions
#set.seed(4322)
#kde <- kde_univariate(x_train= rnorm(100, mean = 4), h = 0.3, kernel = "gaussian", n = 30)
#plot(x = kde$x, y = kde$y,type ="l")

