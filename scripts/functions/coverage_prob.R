coverage_for_simulation_step <- function(n,
                                         x_point,
                                         data_model,
                                         bandwidth_model,
                                         conf_int_model,
                                         kernel,
                                         alpha
){
  
  # select data model for simulations
  if(data_model == "m1"){
    x = rnorm(n, mean = 0, sd = 1)
    f_of_x = dnorm(x_point, mean = 0, sd = 1)
  }
  
  # select bandwidth model for simulations
  if(bandwidth_model == "plug_in_sj"){
    h <- bandwidth_plug_in_sj(x)
  }
  
  # select confidence interval model for simulations
  conf_int <- conf_int(x = x, x_point = x_point, h = h, kernel = kernel, alpha = alpha,
                       model = conf_int_model)
  
  
  check_coverage <- (conf_int[2] > f_of_x) & (conf_int[1]  < f_of_x)
  
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







