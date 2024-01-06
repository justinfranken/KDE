# import libraries
source(file.path(getwd(),"scripts","lib.r"))

# import sources
source(file.path(getwd(),"real_data_application","extract_data.r"))
source(file.path(getwd(),"scripts","functions","kde.r"))
source(file.path(getwd(),"scripts","functions","bandwidth_selection.r"))
source(file.path(getwd(),"scripts","functions","kernel.r"))

# select data
file_of_interest <- "fmli231"
columns_of_interest <- c("FINATXEM")

# extract data
data <- extract_data(
    ce_pumd_file = file_of_interest,
    target_column = columns_of_interest
)

#------------------         select distribution           ----------------------

x <- data$FamilyIncomeAfterTaxes

#----------------------         eval points           --------------------------

eval_points <- seq(min(x),max(x),length.out = 200)

#--------------------------            rbc            --------------------------

kde_rbc <- kde(x = x,
               eval = eval_points,
               h = bandwidth_scott(x),
               kernel = "epanechnikov",
               ci = c("rbc"),
               alpha = 0.05
               )

f_rbc<- kde_rbc$f_m

conf_int_rbc<- kde_rbc$ci[["rbc"]]

#--------------------------            us            --------------------------

kde_us <- kde(x = x,
               eval = eval_points,
               h = bandwidth_scott(x) * 0.7,
               kernel = "epanechnikov",
               ci = c("us"),
               alpha = 0.05
)

f_us <- kde_us$f_k

conf_int_us <- kde_us$ci[["us"]]

#--------------------------           plot            --------------------------

plot_ly() %>%
  add_trace(x = eval_points,
            y = f_rbc,
            type = 'scatter',
            mode = 'lines',
            name = "Robust bias correction",
            colors = "royalblue"
            ) %>%
  add_trace(x = c(eval_points,rev(eval_points)),
            y = c(conf_int_rbc$lower, rev(conf_int_rbc$upper)),
            type = 'scatter',
            mode = 'lines',
            name = "CI 95% (RBC)",
            line = list(width = 0),
            fillcolor = "rgba(65, 105, 225, 0.2)", 
            fill = "toself"
            ) %>%
  add_trace(x = eval_points,
            y = f_us,
            type = 'scatter',
            mode = 'lines',
            name = "Undersmoothing",
            line  = list(color = "#FF8247")
  ) %>%
  add_trace(x = c(eval_points,rev(eval_points)),
            y = c(conf_int_us$lower, rev(conf_int_us$upper)),
            type = 'scatter',
            mode = 'lines',
            name = "CI 95% (US)",
            line = list(width = 0),
            fillcolor = "rgba(178, 34, 34, 0.1)", 
            fill = "toself"
  ) %>%
  layout(title = 'Kernel Density Estimation of Family Income After Taxes',
         xaxis = list(title = '<B>Income<B>', tickvals = seq(0,700,25)*1000),
         yaxis = list(title = '<B>Estimated Density<B>', tickformat = ".4%"),
         legend = list(x = 0.5,y = -0.1,
                       orientation = "h")
  ) 



