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
    x =       rnorm(n, mean = 0, sd = 1)
    f_point = dnorm(x_point, mean = 0, sd = 1)
  }
  
  if(data_model == "m2"){
    # Model 2: Skewed Unimodal Density
    x =     c(rnorm((1/5) * n,       mean = 0, sd = 1) , 
              rnorm((1/5) * n,       mean = 1/2, sd = 2/3) , 
              rnorm((3/5) * n,       mean = 13/12, sd = 5/9)
              )
    f_point = (1/5) * dnorm(x_point, mean = 0, sd = 1) + 
              (1/5) * dnorm(x_point, mean = 1/2, sd = 2/3) + 
              (3/5) * dnorm(x_point, mean = 13/12, sd = 5/9)
  }
  
  if(data_model == "m3"){
    # Model 3: Bimodal Density
     x =     c(rnorm((1/2)*n,         mean = -1, sd = 2/3), 
               rnorm((1/2)*n,         mean = 1, sd = 2/3)
               )
     f_point = (1/2) * dnorm(x_point, mean = -1, sd = 2/3) + 
               (1/2) * dnorm(x_point, mean = 1, sd = 2/3)
  }
  
  if(data_model == "m4"){
    # Model 3: Asymmetric Bimodal Density
    x =       c(rnorm((3/4)*n,       mean = 0,   sd = 1) , 
                rnorm((1/4)*n,       mean = 3/2, sd = 1/3)
                )
    f_point = (3/4) * dnorm(x_point, mean = 0, sd = 1) +
              (1/4) * dnorm(x_point, mean = 3/2, sd = 1/3)
    
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
  conf_int_list <- kde(x = x, eval = x_point, h = h, kernel = kernel, ci = conf_int_model, alpha = alpha)$ci
  
  conf_int <- unlist(conf_int_list)
  
  #-----------------------------------------------------------------------------
  # evaluate coverage of simulation step
  check_coverage <- (conf_int[1]  <= f_point) & (f_point <= conf_int[2])  
  
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
  
  simulations <- sapply((1:S),function(s){
    
    #if (s %% 100 == 0){cat("Simulations:",s,"\n")}
    
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
  
  #cat("Coverage probability:",coverage_prob,"\n")
  
  return(coverage_prob)
}







