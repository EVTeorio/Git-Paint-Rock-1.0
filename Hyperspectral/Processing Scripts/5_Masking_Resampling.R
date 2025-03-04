
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
###################### masking shadow pixels ######################################
# Filter rows 
filtered_data <- data_clean[data_clean$X790.821.nm >= 0.4, ]

write.csv(filtered_data,"C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_masked.csv")

############################ Resampling ###########################################
df <- filtered_data

# Store non-spectral columns
non_spectral_columns <- df[, c("TileNumber", "SpeciesID", "TreeID")]

# Filter and resample the spectral data (assuming filter_bands and df_to_speclib functions are already defined)
df <- filter_bands(df)
df <- df_to_speclib(df, type="spectrolab")
df_resampled <- spectrolab::resample(df, new_bands = seq(398, 999, 5), fwhm = 1)

# Combine the non-spectral columns with the resampled spectral data
df_resampled <- cbind(non_spectral_columns, df_resampled)

#Remove sample_name column
df_resampled <- df_resampled[, !colnames(df_resampled) %in% "sample_name"]

write.csv(df_resampled,"C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_masked_5nm.csv")

