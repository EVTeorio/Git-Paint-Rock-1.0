

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(beepr)
beep(3)


# Read in the CSV file
data <- read.csv(
  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/NewCanopiesMD_Raw.csv")

# Remove rows with NA values
df <- na.omit(data)

write.csv(df,"C:/Users/PaintRock/Documents/Data processing/Hyperspectral/NewCanopiesMD_Clean.csv")



