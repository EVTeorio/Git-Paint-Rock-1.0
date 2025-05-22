
setwd("lecospec")
source("Functions/lecospectR.R")

# Load necessary libraries
library(raster)
library(dplyr)
library(tidyr)
library(stringr)
library(spectrolab)
library(RStoolbox)
library(hyperSpec)
library(beepr)
beep()
###################### masking shadow pixels ######################################
data_clean <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Raw_Spectra.csv")
# Filter rows 
filtered_data <- data_clean[data_clean$X790.821.nm >= 0.3, ]

write.csv(filtered_data,"C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Sunlit_Pixels.csv")

