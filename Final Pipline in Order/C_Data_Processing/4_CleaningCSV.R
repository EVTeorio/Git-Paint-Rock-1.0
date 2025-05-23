

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(beepr)
beep()

data <- spectral_df

# Read in the CSV file
data <- read.csv("E:/Final_Rasters/VI_Leafoff.csv")
beep()
# Remove rows where NA
df <- data[!apply(is.na(data[, -(1:3)]), 1, all), ]
beep(7)
# Write the cleaned data to a new CSV
write.csv(df, "E:/Final_Rasters/VI_Leafoff_clean.csv", row.names = FALSE)
beep(3)


