library(ggplot2)
library(gridExtra) # for arranging plots side by side

set.seed(42) # for reproducibility

# Assuming 'data_vector' contains the data points you wish to plot
data_small_n <- rnorm(n = 300, mean = 0, sd = 1)  # Replace this with your actual data
df_small_n <- data.frame(x = data_small_n)

# Calculate the kernel density estimates with different bandwidths
kde1_small <- density(data_small_n, bw = 1)
kde2_small <- density(data_small_n, bw = 0.4)
kde3_small <- density(data_small_n, bw = 0.04)

# Convert density objects to data frames for ggplot
df_kde1_small <- data.frame(x = kde1_small$x, y = kde1_small$y)
df_kde2_small <- data.frame(x = kde2_small$x, y = kde2_small$y)
df_kde3_small <- data.frame(x = kde3_small$x, y = kde3_small$y)

# True density function for the standard normal distribution
true_density <- function(x) dnorm(x, mean = 0, sd = 1)

# Calculate true density values at the KDE x-values
true_dens1_small <- true_density(df_kde1_small$x)
true_dens2_small <- true_density(df_kde2_small$x)
true_dens3_small <- true_density(df_kde3_small$x)

# Compute MSE for each bandwidth
mse1_small <- mean((df_kde1_small$y - true_dens1_small)^2)
mse2_small <- mean((df_kde2_small$y - true_dens2_small)^2)
mse3_small <- mean((df_kde3_small$y - true_dens3_small)^2)


# Base plot with true PDF for small n
plot_small_n <- ggplot(df_small_n, aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                aes(color = "True Density"), linetype = 1, size = 0.7, alpha = 0.7) +
  geom_line(data = df_kde1_small, aes(x = x, y = y, color = "h = 1"), size = 0.7) +
  geom_line(data = df_kde2_small, aes(x = x, y = y, color = "h = 0.4"), size = 0.7) +
  geom_line(data = df_kde3_small, aes(x = x, y = y, color = "h = 0.04"), size = 0.7) +
  theme_minimal()+
  labs(title = "Kernel Density Estimation for n=300", y = "Probability Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual("", values = c("True Density" = "#000000", "h = 1" = "#FF0000", "h = 0.4" = "#ff740a", "h = 0.04" = "#efce12")) +
  scale_y_continuous(limits = c(NA, 0.6)) +
  theme(legend.position = "bottom") +
  annotate("text", x = -5, y = 0.6, hjust = 0, vjust = 1.0, label = sprintf("MSE (h = 1): %.4f", mse1_small), color = "#000000", size = 3) +
  annotate("text", x = -5, y = 0.57, hjust = 0, vjust = 1.0, label = sprintf("MSE (h = 0.4): %.4f", mse2_small), color = "#000000", size = 3) +
  annotate("text", x = -5, y = 0.54, hjust = 0, vjust = 1.0, label = sprintf("MSE (h = 0.04): %.4f", mse3_small), color = "#000000", size = 3)

# Data with large n
data_large_n <- rnorm(n = 30000, mean = 0, sd = 1)
df_large_n <- data.frame(x = data_large_n)

# Calculate the kernel density estimates with different bandwidths for large n
kde1_large <- density(data_large_n, bw = 1)
kde2_large <- density(data_large_n, bw = 0.4)
kde3_large <- density(data_large_n, bw = 0.04)

# Convert density objects to data frames for ggplot for large n
df_kde1_large <- data.frame(x = kde1_large$x, y = kde1_large$y)
df_kde2_large <- data.frame(x = kde2_large$x, y = kde2_large$y)
df_kde3_large <- data.frame(x = kde3_large$x, y = kde3_large$y)

# Calculate true density values at the KDE x-values
true_dens1_large <- true_density(df_kde1_large$x)
true_dens2_large <- true_density(df_kde2_large$x)
true_dens3_large <- true_density(df_kde3_large$x)

# Compute MSE for each bandwidth
mse1_large <- mean((df_kde1_large$y - true_dens1_large)^2)
mse2_large <- mean((df_kde2_large$y - true_dens2_large)^2)
mse3_large <- mean((df_kde3_large$y - true_dens3_large)^2)

# Base plot with true PDF for large n
plot_large_n <- ggplot(df_large_n, aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                aes(color = "True Density"), linetype = 1, size = 0.7, alpha = 0.7) +
  geom_line(data = data.frame(x = kde1_large$x, y = kde1_large$y), aes(x = x, y = y, color = "h = 1"), size = 0.7) +
  geom_line(data = data.frame(x = kde2_large$x, y = kde2_large$y), aes(x = x, y = y, color = "h = 0.4"), size = 0.7) +
  geom_line(data = data.frame(x = kde3_large$x, y = kde3_large$y), aes(x = x, y = y, color = "h = 0.04"), size = 0.7) +
  theme_minimal()+
  labs(title = "Kernel Density Estimation for n=30000", y = "Probability Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_color_manual("", values = c("True Density" = "#000000", "h = 1" = "#FF0000", "h = 0.4" = "#ff740a", "h = 0.04" = "#efce12")) +
  scale_y_continuous(limits = c(NA, 0.6)) +
  annotate("text", x = -5, y = 0.6, hjust = 0, vjust = 1.0, label = sprintf("MSE (h = 1): %.4f", mse1_large), color = "#000000", size = 3) +
  annotate("text", x = -5, y = 0.57, hjust = 0, vjust = 1.0, label = sprintf("MSE (h = 0.4): %.4f", mse2_large), color = "#000000", size = 3) +
  annotate("text", x = -5, y = 0.54, hjust = 0, vjust = 1.0, label = sprintf("MSE (h = 0.04): %.4f", mse3_large), color = "#000000", size = 3)
  
# Combine and display the plots with custom legend
combined_plot <- grid.arrange(plot_small_n, plot_large_n, ncol = 2)

print(combined_plot)

BLD <- file.path(getwd(),"bld")

if (!dir.exists(BLD)) {
dir.create(BLD, recursive = TRUE)
}

ggsave(filename = file.path(BLD, "var_bias_plot.png"), plot = combined_plot, width = 10, height = 5, dpi = 500)
