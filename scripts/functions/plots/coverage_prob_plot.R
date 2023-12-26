coverage_for_sample_size <- function(data_model,
                                     x_point,
                                     bandwidth_model,
                                     conf_int_model = c("rbc","bc","us"),
                                     kernel = "epanechnikov"
                                     ){
  
  data <- coverage_prob_grid[
    c(
      coverage_prob_grid$data_model == data_model &
      coverage_prob_grid$x_point == x_point &
      coverage_prob_grid$bandwidth_model == bandwidth_model &
      coverage_prob_grid$conf_int_model %in%  conf_int_model &
      coverage_prob_grid$kernel == kernel
    ),
  ]
  
  
  plot <- plot_ly(data, 
          x = ~n, 
          y = ~coverage_prob, 
          color = ~conf_int_model,
          type = 'scatter', 
          mode = 'lines'
  ) %>%
    layout(title = list(
      text = paste0("<B>Model = ", data_model,
                    ", x = ",x_point,
                    ", bandwidth = ", bandwidth_model,"<B>"),
      font = list(size = 12)  
    ),
    xaxis = list(title = list(text = "<B>Sample size<B>",
                              font = list(size = 12)),
                 tickfont = list(size = 12)
                 ),
    yaxis = list(title = list(text = "<B>Estimated coverage probability<B>",
                              font = list(size = 12)),
                 tickfont = list(size = 12)
                 ),
    dragmode = "lasso")
  
  return(plot)
}

#coverage_for_sample_size(data_model = "m1",
#                         x_point = -2,
#                         bandwidth_model = "cv",
#                         conf_int_model = c("bc","rbc"),
#                         kernel = "epanechnikov"
#)
