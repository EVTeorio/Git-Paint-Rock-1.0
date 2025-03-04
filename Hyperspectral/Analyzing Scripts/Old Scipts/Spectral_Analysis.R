



# Load required packages
setwd("lecospec")
source("Functions/lecospectR.R")

require(Polychrome)
require(vegan)
require(glue)
library(dplyr)
library(tidyr)
library(beepr)

beep(7)


# Preview data to ensure proper format
# head(data)

Cleaned_Speclib <-
  read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_clean_speclib.csv")
data <- Cleaned_Speclib

colnames(data)[5:ncol(data)] <- sub("^X", "", colnames(data)[5:ncol(data)])
colnames(data) <- sub(".nm$", "", colnames(data))
data[, 5:ncol(data)] <- lapply(data[, 5:ncol(data)], as.numeric)

# Prepare data for summarizing spectral information
data_tall_Fnc_grp1 <- data %>%
  # Select necessary columns (SpeciesID and Spectral Data)
  dplyr::select(SpeciesID, TileNumber, TreeID, where(is.numeric)) %>%
  # Reshape the dataset to long format (one row per wavelength value for each group)
  pivot_longer(cols = where(is.numeric), names_to = "Wavelength", values_to = "Reflectance") %>%
  # Summarize spectral statistics
  group_by(SpeciesID) %>%
  dplyr::summarise(
    Median_Reflectance = median(Reflectance, na.rm = TRUE),
    Max_Reflectance = max(Reflectance, na.rm = TRUE),
    Min_Reflectance = min(Reflectance, na.rm = TRUE),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875, na.rm = TRUE),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125, na.rm = TRUE),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95, na.rm = TRUE),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05, na.rm = TRUE),
  )

