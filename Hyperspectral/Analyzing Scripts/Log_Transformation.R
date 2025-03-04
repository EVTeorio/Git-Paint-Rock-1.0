
# Load necessary libraries
library(ggplot2)
library(MASS)

# read in data
df <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_clean_speclib.csv")
data <- df
df <- data

# Identify spectral columns (those starting with X)
spectral_columns <- grep("^X\\d{3}\\.\\d{3}\\.nm", colnames(df), value = TRUE)

# Example: assuming you want to plot the column 'X396.345.nm'
col <- "X716.741.nm"
df$X716.741.nm
# Create histogram using ggplot
p_hist <- ggplot(df, aes(x = .data[[col]])) +  # Use .data[[col]] to refer to column dynamically
  geom_histogram(binwidth = 0.001, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
  theme_minimal()
# Show the plot
print(p_hist)


# 1. Apply log transformation to the spectral columns
df[spectral_columns] <- lapply(df[spectral_columns], function(x) log(x - min(x) + 1))

# 2. Square Root Transformation (for positive skewness)
df[spectral_columns] <- lapply(df[spectral_columns], function(x) sqrt(x - min(x) + 1))  # Adding 1 to avoid sqrt(0)

# 3. Box-Cox Transformation (for positive data only)
df[spectral_columns] <- lapply(df[spectral_columns], function(x) {
  # Ensure the data is positive by shifting values if necessary
  x_shifted <- x - min(x) + 1  # Adding 1 to avoid log(0)
  
  # Apply Box-Cox transformation, with lambda estimated from data
  boxcox_result <- boxcox(x_shifted ~ 1, plotit = FALSE)
  
  # Extract lambda value that maximizes the log-likelihood
  lambda <- boxcox_result$x[which.max(boxcox_result$y)]
  
  # Apply Box-Cox transformation using the estimated lambda
  transformed_values <- (x_shifted^lambda - 1) / lambda
  
  return(transformed_values)
})

# Plot all transformed distributions
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Plot original data
hist(data, main = "Original Data", xlab = "Data", col = "skyblue", breaks = 50)

# Plot log-transformed data
hist(log_transformed, main = "Log Transformation", xlab = "Log Transformed Data", col = "lightgreen", breaks = 50)

# Plot square root-transformed data
hist(sqrt_transformed, main = "Square Root Transformation", xlab = "Sqrt Transformed Data", col = "lightcoral", breaks = 50)

# Plot Box-Cox transformed data
hist(boxcox_transformed, main = "Box-Cox Transformation", xlab = "Box-Cox Transformed Data", col = "lightyellow", breaks = 50)

