# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stemtools)

# Generate sample data
set.seed(123) # for reproducibility

# Create a data frame with continuous variables and a nominal grouping variable
sample_data <- data.frame(
  performance = runif(100, min = 0, max = 100), # Continuous performance score
  time = seq(from = 1, to = 100, by = 1), # Continuous time variable
  product = sample(c("Product A", "Product B", "Product C"), 100, replace = TRUE), # Product type
  weight = runif(100, min = 50, max = 500) # Survey weights
)

# Display the first few rows of the data
head(sample_data)

# Plot the data using the stem_plot_line function
stem_plot_line(data = sample_data, item = performance, group = product, weight = weight, scale_y = ggplot2::scale_y_continuous())

check()
