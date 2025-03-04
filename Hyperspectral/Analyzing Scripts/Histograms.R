
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

# read in data
df <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Outliers_Removed.csv")

output_dir <- "E:/Git Paint Rock 1.0/Output"

# Identify spectral columns (those starting with X)
spectral_columns <- grep("^X\\d{3}\\.\\d{3}\\.nm", colnames(df), value = TRUE)

# Function to detect outliers using IQR method
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  outliers <- which(x < lower_bound | x > upper_bound)
  return(outliers)
}

# Create a list to store results for the report
outlier_report <- list()

# Create a final report table summarizing all outliers by spectral column
final_report <- bind_rows(outlier_report, .id = "Spectral_Column")

# Loop through each spectral column and create histograms, box plots, detect outliers
for (col in spectral_columns) {
  
  # Create histogram
  p_hist <- ggplot(df, aes_string(x = col)) +
    geom_histogram(binwidth = 0.001, fill = "lightblue", color = "black") +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme_minimal()
  
  # Save histogram plot to disk
  ggsave(paste0(output_dir, "histogram_", col, ".png"), p_hist)
  
  # Create box-and-whisker plot
  p_box <- ggplot(df, aes_string(x = "SpeciesID", y = col)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(title = paste("Box-and-Whisker Plot of", col), x = "SpeciesID", y = col) +
    theme_minimal()
  
  # Save box plot to disk
  ggsave(paste0(output_dir, "boxplot_", col, ".png"), p_box)
  
  # Find outliers
  outliers <- find_outliers(df[[col]])
  outlier_data <- df[outliers, c("SpeciesID", "TreeID", col)]
  
  # Summarize outliers by SpeciesID and TreeID
  outlier_summary <- outlier_data %>%
    group_by(SpeciesID, TreeID) %>%
    summarise(Outliers = n(), .groups = "drop")
  
  # Store the outlier summary in the report list
  outlier_report[[col]] <- outlier_summary
}
beep(3)


