

library(dplyr)
library(caret)
library(tidyr)
library(stringr)
library(beepr)

beep()

# Read and prepare data
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean_sunlit.csv")
justincase <- spec_chem_canopy
spec_chem_canopy <- na.omit(spec_chem_canopy)

canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

# Filter species with more than 8 canopies
species_counts <- canopies %>%
  group_by(SpeciesID) %>%
  tally() %>%
  filter(n > 8)

canopies_filtered <- canopies %>%
  filter(SpeciesID %in% species_counts$SpeciesID)

# Loop to create 10 balanced samples
balanced_samples <- list()

for (i in 1:10) {
  set.seed(100 + i)  # Different seed for each iteration
  
  train_canopies <- canopies_filtered %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 5) %>%
    ungroup()
  
  test_canopies <- anti_join(canopies_filtered, train_canopies, by = "TreeID")
  
  train_df <- spec_chem_canopy %>%
    filter(TreeID %in% train_canopies$TreeID)
  
  # Ensure factor levels
  train_df$SpeciesID <- as.factor(train_df$SpeciesID)
  
  # Sample 500 pixels per TreeID
  balanced_train_df <- train_df %>%
    group_by(TreeID) %>%
    slice_sample(n = 500, replace = FALSE) %>%
    ungroup()
  
  balanced_samples[[i]] <- balanced_train_df
}

# Loop to write each sample to disk
for (i in 1:10) {
  filename <- paste0("E:/Thesis_Final_Data/Balanced_Dataframes/Balanced_Sample_", i, ".csv")
  write.csv(balanced_samples[[i]], file = filename, row.names = FALSE)
}
beep()
