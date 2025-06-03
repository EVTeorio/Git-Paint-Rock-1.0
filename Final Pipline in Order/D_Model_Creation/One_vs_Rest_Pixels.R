# PIXEL-BASED SAMPLING SCRIPT

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
metrics <- c(
  "Boochs", "Boochs2", "CARI", "Carter", "Carter2", "Carter3", "Carter4", "Carter5", "Carter6",
  "CI", "CI2", "ClAInt", "CRI1", "CRI2", "CRI3", "CRI4", "D1", "D2", "Datt", "Datt2", "Datt3",
  "Datt4", "Datt5", "Datt6", "DD", "DDn", "DPI", "DWSI4", "EGFN", "EGFR", "EVI", "GDVI2",
  "GDVI3", "GDVI4", "GI", "Gitelson", "Gitelson2", "GMI1", "GMI2", "GreenNDVI", "Maccioni",
  "MCARI", "MCARIOSAVI", "MCARI2", "MCARI2OSAVI2", "mND705", "mNDVI", "MPRI", "MSAVI", "mSR",
  "mSR2", "mSR705", "MTCI", "MTVI", "NDVI", "NDVI2", "NDVI3", "NPCI", "OSAVI", "OSAVI2",
  "PARS", "PRI", "PRICI2", "PRInorm", "PSND", "PSRI", "PSSR", "RDVI", "REPLE", "REPLi",
  "SAVI", "SIPI", "SPVI", "SR", "SR1", "SR2", "SR3", "SR4", "SR5", "SR6", "SR7", "SR8", "SRPI",
  "SumDr1", "SumDr2", "TCARI", "TCARIOSAVI", "TCARI2", "TCARI2OSAVI2", "TGI", "TVI",
  "Vogelmann", "Vogelmann2", "Vogelmann3", "Vogelmann4",
  "PAD_0_5_off", "PAD_10_15_off", "PAD_15_20_off", "PAD_20_25_off", "PAD_25_30_off",
  "PAD_30_35_off", "PAD_35_40_off", "PAD_40_45_off", "PAD_45_50_off", "PAD_5_10_off",
  "PAD_0_5_on", "PAD_10_15_on", "PAD_15_20_on", "PAD_20_25_on", "PAD_25_30_on",
  "PAD_30_35_on", "PAD_35_40_on", "PAD_40_45_on", "PAD_45_50_on", "PAD_5_10_on",
  "Seasonal_Occupancy_20_35m")

# Loop for 10 iterations
species_importance_results <- list()
for (i in 1:10) {
  cat("\n--- Pixel-Based Sample", i, "---\n")
  set.seed(100 + i)
  
  # Sample pixels within each species
  train_df <- spec_chem_canopy %>%
    group_by(SpeciesID) %>%
    slice_sample(prop = 0.7) %>%
    ungroup()
  
  test_df <- anti_join(spec_chem_canopy, train_df, by = c("TreeID", "SpeciesID", "X")) %>% 
    group_by(SpeciesID) %>%
    slice_sample(n = 2000, replace = TRUE) %>%
    ungroup()
  
  # Balance training data (200 pixels per TreeID)
  balanced_train_df <- train_df %>%
    group_by(TreeID) %>%
    slice_sample(n = 2000, replace = TRUE) %>%
    ungroup()
  
  balanced_train_df$SpeciesID <- as.factor(balanced_train_df$SpeciesID)
  test_df$SpeciesID <- as.factor(test_df$SpeciesID)
  
  # One-vs-rest classification
  species_list <- unique(balanced_train_df$SpeciesID)
  binary_results <- list()
  
  for (species in species_list) {
    cat("    --> One-vs-rest for species:", species, "\n")
    
    train_binary <- balanced_train_df %>%
      mutate(binary_label = ifelse(SpeciesID == species, "target", "other")) %>%
      select(binary_label, all_of(metrics))
    
    test_binary <- test_df %>%
      mutate(binary_label = ifelse(SpeciesID == species, "target", "other")) %>%
      select(binary_label, all_of(metrics))
    
    train_binary$binary_label <- as.factor(train_binary$binary_label)
    test_binary$binary_label <- as.factor(test_binary$binary_label)
    
    if (length(unique(train_binary$binary_label)) < 2 || length(unique(test_binary$binary_label)) < 2) {
      warning(paste("Not enough classes for", species))
      next
    }
    
    rf_bin <- randomForest(binary_label ~ ., data = train_binary, ntree = 1000, importance = TRUE)
    pred_bin <- predict(rf_bin, test_binary)
    cm_bin <- confusionMatrix(pred_bin, test_binary$binary_label, positive = "target")
    
    binary_results[[as.character(species)]] <- list(
      accuracy = cm_bin$overall["Accuracy"],
      precision = cm_bin$byClass["Precision"],
      recall = cm_bin$byClass["Recall"],
      f1 = cm_bin$byClass["F1"],
      top_features = head(sort(importance(rf_bin)[, "MeanDecreaseGini"], decreasing = TRUE), 15)
    )
  }
  
  species_importance_results[[paste0("Pixel_Sample_", i)]] <- binary_results
}
beep()
saveRDS(species_importance_results, "E:/Thesis_Final_Data/pixel_based_results.rds")
