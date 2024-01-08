data_generator <- function(data_model, n, x_point){
  
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
  
  return(list(x = x, f_point = f_point))
}

