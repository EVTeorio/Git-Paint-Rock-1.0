# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(beepr)

# Read in data
df <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_clean_speclib.csv")

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

# Step 1: Loop over each spectral column to identify outliers and create the report
for (column in spectral_columns) {
  outliers <- find_outliers(df[[column]])
  
  # Store the results for the report if there are outliers
  if (length(outliers) > 0) {
    outlier_report[[column]] <- data.frame(
      Spectral_Column = column,
      Outlier_Index = outliers,
      Outlier_Value = df[outliers, column]
    )
  }
}

#  View the outlier report
outlier_report_df <- do.call(rbind, outlier_report)
print(outlier_report_df)


# Step 2: Remove Ouliers
df_clean <- df

for (column in spectral_columns) {
  if (column %in% names(outlier_report)) {
    # Get the indices of the outliers for this column
    outliers <- outlier_report[[column]]$Outlier_Index
    # Remove the outliers
    df_clean <- df_clean[-outliers, ]
  }
}

# You can now use df_clean for further analysis or save it
write.csv(df_clean, 
          "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Outliers_Removed.csv")
