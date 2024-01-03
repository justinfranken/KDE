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


# kernel density estimation

h_silverman <- bandwidth_silverman(data$FamilyIncomeAfterTaxes)

eval_points <- seq(min(data$FamilyIncomeAfterTaxes),
                max(data$FamilyIncomeAfterTaxes),
                length.out = 200)

kde_result <- kde(x = data$FamilyIncomeAfterTaxes,
                eval = eval_points,
                h = h_silverman)

# plot
kde_family_income_plot <- plot_ly(x = kde_result$eval,
                            y = kde_result$f_k,
                            type = 'scatter',
                            mode = 'lines') %>%
                            layout(title = 'Kernel Density Estimation of Family Income After Taxes',
                                xaxis = list(title = 'Income'),
                                yaxis = list(title = 'Density'))

kde_family_income_plot

# confidence intervals
alpha = 0.05
conf_intervals <- kde(x = data$FamilyIncomeAfterTaxes,
                    eval = eval_points,
                    h = h_silverman,
                    alpha = alpha)$ci