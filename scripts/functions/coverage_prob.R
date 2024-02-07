coverage_for_simulation_step <- function(n,
                                         x_point,
                                         data_model,
                                         bandwidth_model,
                                         eta,
                                         lambda,
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
    h <- bandwidth_silverman(x, lambda = lambda)
  }
  
  #-----------------------------------------------------------------------------
  # select confidence interval model 
  conf_int <- kde(x = x, 
                  eval = x_point, 
                  h = h, 
                  b = h * eta, 
                  kernel = kernel, 
                  ci = conf_int_model, 
                  alpha = alpha
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
                           eta = 1,
                           lambda = 1,
                           conf_int_model,
                           kernel,
                           alpha){
  
  simulations <- lapply((1:S),function(s){
    
    coverage <- coverage_for_simulation_step(n,
                                             x_point,
                                             data_model,
                                             bandwidth_model,
                                             eta,
                                             lambda,
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







