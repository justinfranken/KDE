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
  
  # plug in methods 
  if(bandwidth_model == "plug_in_sj"){
    h <- bandwidth_plug_in_sj(x)
  }
  
  # cv methods
  if(bandwidth_model == "cv"){
    h <- suppressWarnings(bandwidth_cv(x)) 
  }
  
  # rule of thumb methods
  if(bandwidth_model == "scott"){
    h <- bandwidth_scott(x)
  }
  if(bandwidth_model == "silverman"){
    h <- bandwidth_silverman(x)
  }
  
  # halls rule for undersmoothing with different lambdas 
  if(bandwidth_model == "hall_0.3"){
    h <- bandwidth_hall(x, lambda = 0.3)
  }
  if(bandwidth_model == "hall_0.5"){
    h <- bandwidth_hall(x, lambda = 0.5)
  }   
  if(bandwidth_model == "hall_0.7"){
    h <- bandwidth_hall(x, lambda = 0.7)
  } 
  
  # explorative bandwidth ideas for undersmoothing
  if(bandwidth_model == "plug_in_sj_0.3"){
    h <- 0.3 * bandwidth_plug_in_sj(x)
  }
  if(bandwidth_model == "plug_in_sj_0.5"){
    h <- 0.5 * bandwidth_plug_in_sj(x)
  }
  if(bandwidth_model == "plug_in_sj_0.7"){
    h <- 0.7 * bandwidth_plug_in_sj(x)
  }
    
  #-----------------------------------------------------------------------------
  # select confidence interval model 
  conf_int_list <- kde(x = x, eval = x_point, h = h, kernel = kernel, ci = conf_int_model, alpha = alpha)$ci
  
  conf_int <- unlist(conf_int_list)
  
  #-----------------------------------------------------------------------------
  # evaluate coverage of simulation step
  check_coverage <- (conf_int[1]  <= f_point) & (f_point <= conf_int[2])  
  
  return(list(check_coverage = check_coverage,
              ci_lower = conf_int[1],
              ci_upper = conf_int[2])
         )
}

coverage_for_n <- function(n,
                           S,
                          data_model,
                          x_point,
                          bandwidth_model,
                          conf_int_model,
                          kernel,
                          alpha){
  
  simulations <- lapply((1:S),function(s){
    
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
  
  simulations <- bind_rows(simulations)
  
  return(list(coverage_prob = mean(simulations$check_coverage),
              ci_lower = mean(simulations$ci_lower),
              ci_upper = mean(simulations$ci_upper)
              )
         )
}







