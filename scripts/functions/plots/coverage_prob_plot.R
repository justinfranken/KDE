coverage_prob_n_plot <- function(data_model,
                                 x_point,
                                 conf_int_model = c("rbc","bc","us"),
                                 bandwidth_model,
                                 kernel = "epanechnikov"
                                     ){
  
  data <- coverage_prob_grid[
    c(
      coverage_prob_grid$data_model == data_model &
      coverage_prob_grid$x_point == x_point &
      coverage_prob_grid$conf_int_model %in% conf_int_model &
      coverage_prob_grid$bandwidth_model %in% bandwidth_model &
      coverage_prob_grid$kernel == kernel
    ),
  ]
  
  data <- data %>% mutate(combined_model = paste0("ci = ",conf_int_model,
                                                  "\n",
                                                  "bw = ",bandwidth_model)
                          )
  
  plot <- plot_ly(data, 
          x = ~n, 
          y = ~coverage_prob, 
          color = ~combined_model,
          type = 'scatter', 
          mode = 'lines'
  )  %>%
    layout(title = list(
      text = paste0("<B>Model = ", data_model,
                    ", x = ",x_point,"<B>"),
      font = list(size = 12)  
    ),
    xaxis = list(title = list(text = "<B>Sample size<B>",
                              font = list(size = 12)),
                              type = "log", 
                 tickfont = list(size = 12)
                 ),
    yaxis = list(title = list(text = "<B>Estimated coverage probability<B>",
                              font = list(size = 12)),
                 tickfont = list(size = 12)
                 ),
    dragmode = "lasso",
    legend = list(y = -0.15,  # Set y to a negative value to move the legend to the bottom
                  orientation = 'h')
    )
  
  return(plot)
}



