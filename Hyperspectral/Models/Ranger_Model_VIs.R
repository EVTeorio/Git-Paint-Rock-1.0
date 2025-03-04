

library(dplyr)
library(tidyr)
library(ranger)
library(caret)
library(beepr)


# Read in data
spec_chem_canopy <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Outliers_Removed.csv")
colnames(spec_chem_canopy)

canopy_VIs <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_VIs_5nm.csv") 
canopy_VIs <- canopy_VIs %>%
  select(-X398)

# Make a list of band names for data filtering later
band_names <- colnames(spec_chem_canopy[, 6:ncol(spec_chem_canopy)])
VI_names <- colnames(canopy_VIs[, 5:ncol(canopy_VIs)])

# Join spectra with vegetation indices and Set the dataframe name to match the one used in the workflow
spec_chem_canopy <- spec_chem_canopy %>% 
  inner_join(canopy_VIs, by = c( "X" = "X", "TileNumber" = "TileNumber", "SpeciesID" = "SpeciesID", "TreeID" = "TreeID"), keep = FALSE)

