coverage_for_simulation_step <- function(n,
                                         x_point,
                                         data_model,
                                         bandwidth_model,
                                         conf_int_model,
                                         kernel,
                                         alpha
){
  
  #-----------------------------------------------------------------------------
  # Select data generating process
  
  if(data_model == "m1"){
    # Model 1: Gaussian density
    x =       rnorm(n,       mean = 0, sd = 1)
    f_point = dnorm(x_point, mean = 0, sd = 1)
  }
  
  if(data_model == "m2"){
    # Model 2: Skewed Unimodal Density
    x =      (rnorm(n,       mean = 0, sd = 1) + rnorm(n,       mean = 1/2, sd = 2/3) + 3*rnorm(n,       mean = 13/12, sd = 5/9))/5
    f_point= (dnorm(x_point, mean = 0, sd = 1) + dnorm(x_point, mean = 1/2, sd = 2/3) + 3*dnorm(x_point, mean = 13/12, sd = 5/9))/5
  }
  
  if(data_model == "m3"){
    # Model 3: Bimodal Density
    x =       (rnorm(n,       mean = -1, sd = 2/3) + rnorm(n,       mean = 1, sd = 2/3))/2
    f_point = (dnorm(x_point, mean = -1, sd = 2/3) + dnorm(x_point, mean = 1, sd = 2/3))/2
  }
  
  if(data_model == "m4"){
    # Model 3: Bimodal Density
    x =       (rnorm(n,       mean = -1, sd = 2/3) + rnorm(n,       mean = 1, sd = 2/3))/2
    f_point = (dnorm(x_point, mean = -1, sd = 2/3) + dnorm(x_point, mean = 1, sd = 2/3))/2
  }
  
  if(data_model == "m4"){
    # Model 3: Asymmetric Bimodal Density
    x =       3*(rnorm(n,       mean = 0, sd = 1) + rnorm(n,       mean = 3/2, sd = 1/3))/4
    f_point = 3*(dnorm(x_point, mean = 0, sd = 1) + dnorm(x_point, mean = 3/2, sd = 1/3))/4
  }
 
  #-----------------------------------------------------------------------------
  # select bandwidth model 
  
  if(bandwidth_model == "plug_in_sj"){
    h <- bandwidth_plug_in_sj(x)
  }
  if(bandwidth_model == "cv"){
    h <- suppressWarnings(bandwidth_cv(x)) 
  }
  if(bandwidth_model == "scott"){
    h <- bandwidth_scott(x)
  }
  if(bandwidth_model == "silverman"){
    h <- bandwidth_silverman(x)
  }
  
  #-----------------------------------------------------------------------------
  # select confidence interval model 
  conf_int <- conf_int(x = x, x_point = x_point, h = h, kernel = kernel, alpha = alpha,
                       model = conf_int_model) # model
  
  #-----------------------------------------------------------------------------
  # evaluate coverage of simulation step
  check_coverage <- (conf_int[2] > f_point) & (conf_int[1]  < f_point)
  
  return(check_coverage)
}

coverage_for_n <- function(n,
                           S,
                          data_model,
                          x_point,
                          bandwidth_model,
                          conf_int_model,
                          kernel,
                          alpha){
  
  cat("Esimtation of coverage probability for data_model =",data_model,"and n =",n,"\n")
  
  simulations <- sapply((1:S),function(s){
    
    if (s %% 100 == 0){cat("Simulations:",s,"\n")}
    
    coverage <- coverage_for_simulation_step(n,
                                             x_point,
                                             data_model,
                                             bandwidth_model,
                                             conf_int_model,
                                             kernel,
                                             alpha  
    )
    return(coverage)
    
  })
  
  coverage_prob <- mean(simulations)
  
  cat("Coverage probability:",coverage_prob,"\n")
  
  return(coverage_prob)
}







