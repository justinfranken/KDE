coverage_prob_n_plot <- function(data,
                                 data_model,
                                 x_point,
                                 conf_int_model = c("rbc","bc","us"),
                                 bandwidth_model,
                                 kernel = "epanechnikov",
                                 x_axis_log = TRUE
                                 ){
  
  if (x_axis_log == TRUE){
    x_axis_type <- "log"
  }
  if (x_axis_log == FALSE){
    x_axis_type <- NULL
  }
  
  data <- data[
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
  ) %>% add_segments(x = min(data$n), xend = max(data$n), 
                     y = 0.95, yend = 0.95, color = "grey",
                     line = list(dash = "dash", color = "grey", width = 0.8),
                     legendgroup = "ci",
                     name = "95% CI"
  ) %>%
    layout(title = list(
      text = paste0("<B>Model = ", data_model,
                    ", x = ",x_point,"<B>"),
      font = list(size = 12)  
    ),
    xaxis = list(title = list(text = "<B>Sample size<B>",
                              font = list(size = 12)),
                              type = x_axis_type, 
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

coverage_prob_n_plot_all_points <- function(data,
                                            data_model,
                                            x_point,
                                            conf_int_model = c("rbc","bc","us"),
                                            bandwidth_model,
                                            kernel = "epanechnikov"
){
  
  data <- coverage_prob_grid[
    c(
      coverage_prob_grid$data_model == data_model &
        coverage_prob_grid$conf_int_model %in% conf_int_model &
        coverage_prob_grid$bandwidth_model %in% bandwidth_model &
        coverage_prob_grid$kernel == kernel
    ),
  ]
  
  data <- data %>% mutate(combined_model = paste0("ci = ",conf_int_model,
                                                  ", ",
                                                  "bw = ",bandwidth_model)
  )
  
  annotations = list( 
    list( 
      x = 0.23,  
      y = 1,  
      text = "<B>x = -4<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.78,  
      y = 1,  
      text = "<B>x =  4<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.23,  
      y = 0.6,  
      text = "<B>x = -2<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),
    list( 
      x = 0.78,  
      y = 0.6,  
      text = "<B>x = 2<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),
    list( 
      x = 0.23,  
      y = 0.26,  
      text = "<B>x = 0<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    )
  )

  #----------------  minus 4
  plot_x_minus_4 <- plot_ly(data[data$x_point == -4,], 
                            x = ~n, 
                            y = ~coverage_prob, 
                            color = ~combined_model,
                            type = 'scatter', 
                            mode = 'lines',
                            legendgroup = ~combined_model,
                            showlegend = F
  ) %>% add_segments(x = min(data$n), xend = max(data$n), 
                     y = 0.95, yend = 0.95, color = "grey",
                     line = list(dash = "dash", color = "grey", width = 0.8),
                     legendgroup = "ci",
                     name = "95% CI"
   ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                          font = list(size = 10)),
                             tickfont = list(size = 10)
                             ),
                yaxis = list(title = list(text = "<B>Coverage probability<B>",
                                          font = list(size = 10)),
                             tickfont = list(size = 10)
                             )
                )
  
  #----------------  plus 4
  
  plot_x_plus_4 <- plot_ly(data[data$x_point == 4,], 
                           x = ~n, 
                           y = ~coverage_prob, 
                           color = ~combined_model,
                           type = 'scatter', 
                           mode = 'lines',
                           legendgroup = ~combined_model,
                           showlegend = F
  ) %>% add_segments(x = min(data$n), xend = max(data$n), 
                     y = 0.95, yend = 0.95, color = "grey",
                     line = list(dash = "dash", color = "grey", width = 0.8),
                     legendgroup = "ci",
                     name = "95% CI"
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = 10)),
                            tickfont = list(size = 10)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = 10)),
               tickfont = list(size = 10)
  )
  )
  
  #----------------  minus 2
  
  plot_x_minus_2 <- plot_ly(data[data$x_point == -2,], 
                            x = ~n, 
                            y = ~coverage_prob, 
                            color = ~combined_model,
                            type = 'scatter', 
                            mode = 'lines',
                            legendgroup = ~combined_model,
                            showlegend = F
  ) %>% add_segments(x = min(data$n), xend = max(data$n), 
                     y = 0.95, yend = 0.95, color = "grey",
                     line = list(dash = "dash", color = "grey", width = 0.8),
                     legendgroup = "ci",
                     name = "95% CI"
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = 10)),
                            tickfont = list(size = 10)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = 10)),
               tickfont = list(size = 10)
  )
  )
  
  #----------------  plus 2
  
  plot_x_plus_2 <- plot_ly(data[data$x_point == 2,], 
                           x = ~n, 
                           y = ~coverage_prob, 
                           color = ~combined_model,
                           type = 'scatter', 
                           mode = 'lines',
                           legendgroup = ~combined_model,
                           showlegend = F
  ) %>% add_segments(x = min(data$n), xend = max(data$n), 
                     y = 0.95, yend = 0.95, color = "grey",
                     line = list(dash = "dash", color = "grey", width = 0.8),
                     legendgroup = "ci",
                     name = "95% CI"
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = 10)),
                            tickfont = list(size = 10)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = 10)),
               tickfont = list(size = 10)
  )
  )
  
  #----------------  0
  
  plot_x_0 <- plot_ly(data[data$x_point == 0,], 
                      x = ~n, 
                      y = ~coverage_prob, 
                      color = ~combined_model,
                      type = 'scatter', 
                      mode = 'lines',
                      legendgroup = ~combined_model,
                      showlegend = T
  ) %>% add_segments(x = min(data$n), xend = max(data$n), 
                     y = 0.95, yend = 0.95, color = "grey",
                     line = list(dash = "dash", color = "grey", width = 0.8),
                     legendgroup = "ci",
                     name = "95% CI"
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = 10)),
                            tickfont = list(size = 10)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = 10)),
               tickfont = list(size = 10)
  )
  )
  
  #----------------  combine subplots
  
  plot <- subplot(plot_x_minus_4,
                  plot_x_plus_4,
                  plot_x_minus_2,
                  plot_x_plus_2,
                  plot_x_0, 
                  nrows = 3,
                  margin = 0.07,
                  titleX = T,
                  titleY = T
                  ) %>% 
    layout(title = paste0("Distribution =  ", data_model),
           annotations = annotations,
           legend = list(x = 0.5,y = -0.05,
                         orientation = "v"
                         )
    )
  
  
  return(plot)
}





