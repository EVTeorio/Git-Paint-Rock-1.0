
# OBJECT-BASED SAMPLING SCRIPT

library(dplyr)
library(stringr)
library(randomForest)
library(caret)
library(beepr)

# Load data
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean_sunlit_no_nas.csv")
beep()
str(spec_chem_canopy)

# Keep species with >5 canopies
canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

species_counts <- canopies %>%
  count(SpeciesID) %>%
  filter(n > 5)

spec_chem_canopy <- spec_chem_canopy %>%
  filter(SpeciesID %in% species_counts$SpeciesID)

# Define metrics
metrics <- c("PAD_0_5_off", "PAD_10_15_off", "PAD_15_20_off", "PAD_20_25_off", "PAD_25_30_off",
             "PAD_30_35_off", "PAD_35_40_off", "PAD_40_45_off", "PAD_45_50_off", "PAD_5_10_off",
             "PAD_0_5_on", "PAD_10_15_on", "PAD_15_20_on", "PAD_20_25_on", "PAD_25_30_on",
             "PAD_30_35_on", "PAD_35_40_on", "PAD_40_45_on", "PAD_45_50_on", "PAD_5_10_on",
             "Seasonal_Occupancy_20_35m")

# Precompute canopy-averaged data
canopy_means <- spec_chem_canopy %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE), .groups = "drop")

# Loop over 10 iterations
species_importance_results <- list()
for (i in 1:10) {
  cat("\n--- Object-Based Sample", i, "---\n")
  set.seed(100 + i)
  
  # Sample 4 train, 2 test canopies per species
  train_canopies <- canopy_means %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 4) %>%
    ungroup()
  
  test_canopies <- anti_join(canopy_means, train_canopies, by = "TreeID") %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 2) %>%
    ungroup()
  
  train_canopies$SpeciesID <- as.factor(train_canopies$SpeciesID)
  test_canopies$SpeciesID <- as.factor(test_canopies$SpeciesID)
  
  species_list <- unique(train_canopies$SpeciesID)
  binary_results <- list()
  
  for (species in species_list) {
    cat("    --> One-vs-rest for species:", species, "\n")
    
    train_binary <- train_canopies %>%
      mutate(binary_label = ifelse(SpeciesID == species, "target", "other")) %>%
      select(binary_label, all_of(metrics))
    
    test_binary <- test_canopies %>%
      mutate(binary_label = ifelse(SpeciesID == species, "target", "other")) %>%
      select(binary_label, all_of(metrics))
    
    train_binary$binary_label <- as.factor(train_binary$binary_label)
    test_binary$binary_label <- as.factor(test_binary$binary_label)
    
    if (length(unique(train_binary$binary_label)) < 2 || length(unique(test_binary$binary_label)) < 2) {
      warning(paste("Not enough data for species", species))
      next
    }
    
    rf_bin <- randomForest(binary_label ~ ., data = train_binary, ntree = 1000, importance = TRUE)
    pred_bin <- predict(rf_bin, test_binary)
    cm_bin <- confusionMatrix(pred_bin, test_binary$binary_label, positive = "target")
    
    imp_bin <- importance(rf_bin)[, "MeanDecreaseGini", drop = FALSE]
    imp_bin <- sort(imp_bin[, 1], decreasing = TRUE)
    top_features <- head(imp_bin, 15)
    
    binary_results[[as.character(species)]] <- list(
      accuracy = cm_bin$overall["Accuracy"],
      precision = cm_bin$byClass["Precision"],
      recall = cm_bin$byClass["Recall"],
      f1 = cm_bin$byClass["F1"],
      top_features = top_features
    )
  }
  
  species_importance_results[[paste0("Object_Sample_", i)]] <- binary_results
}

beep()
saveRDS(species_importance_results, "E:/Thesis_Final_Data/object_based_results.rds")
