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
  data <- data_generator(data_model = data_model,
                         n = n,
                         x_point = x_point)
  
  # save simulated distribution in x
  x <- data$x
  
  # save true value of distribution for x_point in f_point
  f_point <- data$f_point
  
  # remove data
  rm(data)
  
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
  if(bandwidth_model == "silverman"){
    h <- bandwidth_silverman(x)
  }
  
  # halls rule for undersmoothing with different lambdas 
  if(bandwidth_model == "silverman_0.1"){
    h <- 0.1 * bandwidth_silverman(x)
  }
  
  if(bandwidth_model == "silverman_0.3"){
    h <- 0.3 * bandwidth_silverman(x)
  }
  if(bandwidth_model == "silverman_0.5"){
    h <- 0.5 * bandwidth_silverman(x)
  }   
  if(bandwidth_model == "silverman_0.7"){
    h <- 0.7 * bandwidth_silverman(x)
  } 
  if(bandwidth_model == "silverman_1.0"){
    h <- bandwidth_silverman(x)
  } 
  #-----------------------------------------------------------------------------
  # select confidence interval model 
  conf_int <- kde(x = x, eval = x_point, h = h, kernel = kernel, ci = conf_int_model, alpha = alpha
                  )$ci[[conf_int_model]]
  
  #-----------------------------------------------------------------------------
  # evaluate coverage of simulation step
  check_coverage <- (conf_int$lower  <= f_point) & (f_point <= conf_int$upper)  
  
  return(list(check_coverage = check_coverage,
              ci_lower = conf_int$lower,
              ci_upper = conf_int$upper)
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







