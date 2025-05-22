

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(beepr)
beep()


data <- spectral_df

# Read in the CSV file
data <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/NewCanopiesMD_Raw.csv")

# Remove rows where NA
df <- data[!apply(is.na(data[, -(1:3)]), 1, all), ]

# Write the cleaned data to a new CSV
write.csv(df, "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Fusion_HSI_Leafoff_clean.csv", row.names = FALSE)
beep()
