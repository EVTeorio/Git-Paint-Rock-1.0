
library(raster)
library(dplyr)
library(caret)
library(tidyr)
library(stringr)
library(spectrolab)
library(RStoolbox)
library(hyperSpec)
library(beepr)
beep()


spec_chem_canopy <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Sunlit_Pixels.csv")
str(spec_chem_canopy)


# Step 1: Create a canopy-level dataframe (1 row per TreeID)
# This ensures we only use one row per canopy for splitting
canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

# Step 2: Remove SpeciesID groups that only have one canopy
species_counts <- canopies %>%
  group_by(SpeciesID) %>%
  tally() %>%
  filter(n > 1)

# Keep only TreeIDs with species that occur more than once
canopies_filtered <- canopies %>%
  filter(SpeciesID %in% species_counts$SpeciesID)

# Step 3: Stratified 50/50 split by SpeciesID
set.seed(42)
split_idx <- createDataPartition(canopies_filtered$SpeciesID, p = 0.5, list = FALSE)

train_canopies <- canopies_filtered[split_idx, ]
test_canopies <- canopies_filtered[-split_idx, ]

# Step 4: Get full pixel data for each canopy
train_df <- spec_chem_canopy %>% filter(TreeID %in% train_canopies$TreeID)
test_df <- spec_chem_canopy %>% filter(TreeID %in% test_canopies$TreeID)




