

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(beepr)
beep()


# Read in the CSV file
data <- read.csv(
  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/NewCanopiesMD_Raw.csv")
data <- spectral_df
# Remove rows with NA values
df <- na.omit(data)

write.csv(df,"C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Raw_Spectra.csv")



