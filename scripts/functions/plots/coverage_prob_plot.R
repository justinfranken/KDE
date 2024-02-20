coverage_prob_n_plot <- function(data,
                                 data_model,
                                 x_point,
                                 conf_int_model = c("rbc","bc","us"),
                                 bandwidth_model,
                                 lambda = 1,
                                 eta = 1, # b = h
                                 kernel = "epanechnikov"
                                 ){
  
  data <- data[
    c(
      coverage_prob_grid$data_model == data_model &
      coverage_prob_grid$x_point == x_point &
      coverage_prob_grid$conf_int_model %in% conf_int_model &
      coverage_prob_grid$eta %in% eta &
      coverage_prob_grid$lambda %in% lambda &
      coverage_prob_grid$bandwidth_model %in% bandwidth_model &
      coverage_prob_grid$kernel == kernel
    ),
  ]
  
  data <- data %>% mutate(combined_model = paste0("ci = ",conf_int_model,
                                                  "\n",
                                                  "bw = ",bandwidth_model,
                                                  "\n",
                                                  "(\u03BB = ",lambda,", \u03B7 = ",eta,")"
                                                  )
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
                                            lambda = 1,
                                            eta = 1,
                                            kernel = "epanechnikov"
){
  
  data <- data[
    c(
      coverage_prob_grid$data_model == data_model &
      coverage_prob_grid$conf_int_model %in% conf_int_model &
      coverage_prob_grid$eta %in% eta &
      coverage_prob_grid$lambda %in% lambda &
      coverage_prob_grid$bandwidth_model %in% bandwidth_model &
      coverage_prob_grid$kernel == kernel
    ),
  ]
  
  data <- data %>% mutate(combined_model = case_when(
    conf_int_model == "us" ~ paste0("ci = ",conf_int_model,
                                      ", bw = ",bandwidth_model,
                                      " (","\u03BB = ",lambda,")"
                                     ),
    conf_int_model != "us" ~ paste0("ci = ",conf_int_model,
                                    ", bw = ",bandwidth_model,
                                    " (","\u03B7 = ",eta,")"
                                     )
    )
  )
  
  y_axis_range <- c(0.6,1)
  y_axis_tickvals <- seq(0.6,1,0.1)
  height = 450
  text_size = 9
  legend_size = 9
  
  annotations = list( 
    list( 
      x = 0.2,  
      y = 1,  
      text = "<B>x = -2<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.78,  
      y = 1,  
      text = "<B>x =  2<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.2,  
      y = 0.6,  
      text = "<B>x = -1<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),
    list( 
      x = 0.78,  
      y = 0.6,  
      text = "<B>x = 1<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),
    list( 
      x = 0.2,  
      y = 0.26,  
      text = "<B>x = 0<B>",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    )
  )
  
  #----------------  minus 2
  plot_x_minus_2 <- plot_ly(data[data$x_point == -2,],
                            height = height,
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
                     showlegend = F,
                     name = "95% CI"
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range,
               tickvals = y_axis_tickvals
  )
  )
  
  #----------------  plus 2
  
  plot_x_plus_2 <- plot_ly(data[data$x_point == 2,],
                           height = height, 
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
                     showlegend = F,
                     name = "95% CI"
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range,
               tickvals = y_axis_tickvals
  )
  )
  
  #----------------  minus 1
  
  plot_x_minus_1 <- plot_ly(data[data$x_point == -1,],
                            height = height,  
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
                     showlegend = F,
                     name = "95% CI"
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range,
               tickvals = y_axis_tickvals
  )
  )
  
  #----------------  plus 1
  
  plot_x_plus_1 <- plot_ly(data[data$x_point == 1,],
                           height = height, 
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
                     showlegend = F,
                     name = "95% CI"
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range,
               tickvals = y_axis_tickvals
  )
  )
  
  #----------------  0
  
  plot_x_0 <- plot_ly(data[data$x_point == 0,],
                      height = height,  
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
                     name = "95% CI",
                     showlegend = F
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Coverage probability<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range,
               tickvals = y_axis_tickvals
  )
  )
  
  #----------------  combine subplots
  
  plot <- subplot(plot_x_minus_2,
                  plot_x_plus_2,
                  plot_x_minus_1,
                  plot_x_plus_1,
                  plot_x_0, 
                  nrows = 3,
                  margin = 0.09,
                  titleX = T,
                  titleY = T
  ) %>% 
    layout(title = paste0("Model ", substring(data_model,2,2)),
           annotations = annotations,
           legend = list(x = 0.5,y = -0.05,
                         orientation = "v",
                         tracegroupgap = 1,
                         font = list(size = legend_size)
           )
    )
  
  
  return(plot)
}
