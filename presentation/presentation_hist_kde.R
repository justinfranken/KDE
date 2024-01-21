# Load the necessary library
library(ggplot2)

# Define the custom Gaussian kernel function
gaussian_kernel_adjust <- function(x, data_point, bw){
  return((1 / (length(data) * bw * sqrt(2 * pi))) * exp(-(1/2) * ((x - data_point) / bw)^2))
}

# Generate a small sample of n=6
set.seed(42) # for reproducibility
data <- runif(n=5, min=-5, max=5)

# Create the base plot with the data
base_plot <- ggplot(data.frame(x = data), aes(x))
kde_data <- density(data)
bw <- kde_data$bw

hist_plot <- base_plot +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = 'skyblue', colour = 'darkblue', alpha = 0.7) +
  geom_rug(sides = "b", colour = 'darkblue') +
  labs(x = 'x', y = 'Probability Density', title = 'Histogram') +
  xlim(c(min(data) - 2, max(data) + 2)) +
  ylim(c(0, 0.3)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10))

kde_plot <- base_plot +
  geom_line(data = data.frame(x = kde_data$x, y = kde_data$y), aes(x = x, y = y), colour = "blue", size = 1) +
  geom_rug(sides = "b", colour = 'darkblue') +
  labs(x = 'x', y = 'Probability Density', title = 'Kernel Density Estimate') +
  xlim(c(min(data) - 2, max(data) + 2)) +
  ylim(c(0, 0.3)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 10))

for (i in 1:length(data)) {
  kde_plot <- kde_plot + 
    stat_function(fun = gaussian_kernel_adjust, args = list(data_point = data[i], bw = kde_data$bw),
                  colour = 'red', linetype = 'dashed', size = 0.5)
}

combined_plot <- grid.arrange(hist_plot, kde_plot, ncol = 2)

print(combined_plot)


BLD <- file.path(getwd(),"bld")

if (!dir.exists(BLD)) {
dir.create(BLD, recursive = TRUE)
}

ggsave(filename = file.path(BLD, "hist_kde_plot.png"), plot = combined_plot, width = 10, height = 5, dpi = 300)
