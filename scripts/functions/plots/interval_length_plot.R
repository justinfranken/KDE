
interval_length_n_plot_all_points <- function(data,
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
  ,
  interval_length = ifelse(ci_upper>0,ci_upper,0) - ifelse(ci_lower>0,ci_lower,0)
  
  )
  
  y_axis_range <- c(min(data$interval_length),
                    max(data$interval_length))
  
  height = 450
  text_size = 9
  
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
                            y = ~interval_length, 
                            color = ~combined_model,
                            type = 'scatter', 
                            mode = 'lines',
                            legendgroup = ~combined_model,
                            showlegend = F
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Interval length<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range
  )
  )
  
  #----------------  plus 2
  
  plot_x_plus_2 <- plot_ly(data[data$x_point == 2,], 
                           height = height,
                           x = ~n, 
                           y = ~interval_length, 
                           color = ~combined_model,
                           type = 'scatter', 
                           mode = 'lines',
                           legendgroup = ~combined_model,
                           showlegend = F
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Interval length<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range
  )
  )
  
  #----------------  minus 1
  
  plot_x_minus_1 <- plot_ly(data[data$x_point == -1,], 
                            height = height,
                            x = ~n, 
                            y = ~interval_length, 
                            color = ~combined_model,
                            type = 'scatter', 
                            mode = 'lines',
                            legendgroup = ~combined_model,
                            showlegend = F
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Interval length<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range
  )
  )
  
  #----------------  plus 1
  
  plot_x_plus_1 <- plot_ly(data[data$x_point == 1,], 
                           height = height,
                           x = ~n, 
                           y = ~interval_length, 
                           color = ~combined_model,
                           type = 'scatter', 
                           mode = 'lines',
                           legendgroup = ~combined_model,
                           showlegend = F
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Interval length<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range
  )
  )
  
  #----------------  0
  
  plot_x_0 <- plot_ly(data[data$x_point == 0,], 
                      height = height,
                      x = ~n, 
                      y = ~interval_length, 
                      color = ~combined_model,
                      type = 'scatter', 
                      mode = 'lines',
                      legendgroup = ~combined_model,
                      showlegend = T
  ) %>% layout(xaxis = list(title = list(text = "<B>Sample size<B>",
                                         font = list(size = text_size)),
                            tickfont = list(size = text_size)
  ),
  yaxis = list(title = list(text = "<B>Interval length<B>",
                            font = list(size = text_size)),
               tickfont = list(size = text_size),
               range = y_axis_range
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
                         font = list(size = 10)
           )
    )
  
  
  return(plot)
}
