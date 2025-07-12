

library(dplyr)
library(stringr)
library(randomForest)
library(caret)
library(beepr)
library(tibble)
library(tidyverse)

# Load data
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean_sunlit_no_nas.csv")
beep()
str(spec_chem_canopy)

spec_chem_canopy <- data_clean
# Get canopy and species info
canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

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
  "Vogelmann", "Vogelmann2", "Vogelmann3", "Vogelmann4"
)

# Define vegetation indices (VIs)
vi_vars <- c(
  "Boochs", "Boochs2", "CARI", "Carter", "Carter2", "Carter3", "Carter4", "Carter5", "Carter6",
  "CI", "CI2", "ClAInt", "CRI1", "CRI2", "CRI3", "CRI4", "D1", "D2", "Datt", "Datt2", "Datt3",
  "Datt4", "Datt5", "Datt6", "DD", "DDn", "DPI", "DWSI4", "EGFN", "EGFR", "EVI", "GDVI2",
  "GDVI3", "GDVI4", "GI", "Gitelson", "Gitelson2", "GMI1", "GMI2", "GreenNDVI", "Maccioni",
  "MCARI", "MCARIOSAVI", "MCARI2", "MCARI2OSAVI2", "mND705", "mNDVI", "MPRI", "MSAVI", "mSR",
  "mSR2", "mSR705", "MTCI", "MTVI", "NDVI", "NDVI2", "NDVI3", "NPCI", "OSAVI", "OSAVI2",
  "PARS", "PRI", "PRICI2", "PRInorm", "PSND", "PSRI", "PSSR", "RDVI", "REPLE", "REPLi",
  "SAVI", "SIPI", "SPVI", "SR", "SR1", "SR2", "SR3", "SR4", "SR5", "SR6", "SR7", "SR8", "SRPI",
  "SumDr1", "SumDr2", "TCARI", "TCARIOSAVI", "TCARI2", "TCARI2OSAVI2", "TGI", "TVI",
  "Vogelmann", "Vogelmann2", "Vogelmann3", "Vogelmann4"
)


# Identify LiDAR variables
leafon_vars <- c("PAD_0_5_on", "PAD_10_15_on", "PAD_15_20_on", "PAD_20_25_on", "PAD_25_30_on",
                 "PAD_30_35_on", "PAD_35_40_on","PAD_5_10_on")

leafoff_vars <- c("PAD_0_5_off", "PAD_10_15_off", "PAD_15_20_off", "PAD_20_25_off", "PAD_25_30_off",
                  "PAD_30_35_off", "PAD_35_40_off", "PAD_5_10_off")

seasonal_var <- "Seasonal_Occupancy_20_35m"

model_inputs <- list(
  VIs_only = vi_vars,
  VIs_LiDARleafon = c(vi_vars, leafon_vars),
  VIs_LiDARleafoff = c(vi_vars, leafoff_vars),
  VIs_allLiDAR = c(vi_vars, leafon_vars, leafoff_vars, seasonal_var)
)

# Average metrics by canopy
canopy_means <- spec_chem_canopy %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE), .groups = "drop")

# Precompute canopy means for model input groups
grouped_canopy_means <- list()
for (group_name in names(model_inputs)) {
  grouped_canopy_means[[group_name]] <- spec_chem_canopy %>%
    group_by(TreeID, SpeciesID) %>%
    summarise(across(all_of(model_inputs[[group_name]]), mean, na.rm = TRUE), .groups = "drop")
}

# Store results
all_results <- list()
importance_results <- list()
grouped_accuracy_results <- list()

for (i in 1:50) {
  cat("\n--- Object-Based Sample", i, "---\n")
  set.seed(50 + i)
  
  species_counts <- canopy_means %>% count(SpeciesID)
  
  species_12_plus <- species_counts %>% filter(n >= 12) %>% pull(SpeciesID)
  species_6_11 <- species_counts %>% filter(n >= 6 & n < 12) %>% pull(SpeciesID)
  
  # Sample 12 canopies per species with >=12
  sampled_12_plus <- canopy_means %>%
    filter(SpeciesID %in% species_12_plus) %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 12) %>%
    ungroup()
  
  train_12_plus <- sampled_12_plus %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 8) %>%
    ungroup()
  test_12_plus <- sampled_12_plus %>%
    filter(!TreeID %in% train_12_plus$TreeID)
  
  # Sample 6 canopies per species with 6-11
  sampled_6_11 <- canopy_means %>%
    filter(SpeciesID %in% species_6_11) %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 6) %>%
    ungroup()
  
  train_6_11 <- sampled_6_11 %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 4) %>%
    ungroup()
  test_6_11 <- sampled_6_11 %>%
    filter(!TreeID %in% train_6_11$TreeID)
  
  # Rare species <6 canopies
  rare_species <- canopy_means %>%
    filter(!(SpeciesID %in% c(species_12_plus, species_6_11)))
  
  # From rare species, select 1 canopy per species for training, relabel as "others"
  rare_train <- rare_species %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(SpeciesID = "others")
  
  # From rare species, select 1 *different* canopy per species for testing, relabel as "others"
  rare_test <- rare_species %>%
    filter(!(TreeID %in% rare_train$TreeID)) %>%
    # group_by(SpeciesID) %>%
    # slice_sample(n = 1) %>%
    # ungroup() %>%
    mutate(SpeciesID = "others")
  
  # Final train and test sets
  train_canopies <- bind_rows(train_12_plus, train_6_11, rare_train)
  test_canopies <- bind_rows(test_12_plus, test_6_11, rare_test)
  
  train_treeIDs <- train_canopies$TreeID
  test_treeIDs <- test_canopies$TreeID
  
  # Prepare training data - relabeled already
  train_data_full <- train_canopies %>%
    select(SpeciesID, all_of(metrics)) %>%
    drop_na()
  train_data_full$SpeciesID <- factor(train_data_full$SpeciesID)
  
  # Prepare testing data - relabeled already
  test_data_full <- test_canopies %>%
    select(SpeciesID, all_of(metrics)) %>%
    drop_na()
  test_data_full$SpeciesID <- factor(test_data_full$SpeciesID, levels = levels(train_data_full$SpeciesID))
  
  # Train RF model
  rf_model_full <- randomForest(SpeciesID ~ ., data = train_data_full, ntree = 3000, importance = TRUE)
  
  # Predict on test data
  preds_full <- predict(rf_model_full, newdata = test_data_full)
  
  # Confusion matrix including "others" as a single class
  cm_full <- confusionMatrix(preds_full, test_data_full$SpeciesID)
  
  precision <- cm_full$byClass[, "Precision"]
  recall <- cm_full$byClass[, "Recall"]
  f1_by_class <- 2 * (precision * recall) / (precision + recall)
  f1_macro <- mean(f1_by_class, na.rm = TRUE)
  
  all_results[[paste0("Sample_", i)]] <- list(
    model = rf_model_full,
    confusion = cm_full,
    accuracy = cm_full$overall["Accuracy"],
    f1_by_class = f1_by_class,
    f1_macro = f1_macro
  )
  
  importance_results[[paste0("Sample_", i)]] <- importance(rf_model_full)
  
  # ====== Grouped models ======
  group_iteration_results <- list()
  
  for (group_name in names(model_inputs)) {
    features <- model_inputs[[group_name]]
    group_means <- grouped_canopy_means[[group_name]]
    
    train_data <- group_means %>%
      filter(TreeID %in% train_treeIDs) %>%
      select(SpeciesID, all_of(features)) %>%
      drop_na()
    train_data$SpeciesID <- factor(train_data$SpeciesID)
    
    test_data <- group_means %>%
      filter(TreeID %in% test_treeIDs) %>%
      select(SpeciesID, all_of(features)) %>%
      drop_na()
    test_data$SpeciesID <- factor(test_data$SpeciesID, levels = levels(train_data$SpeciesID))
    
    rf_group_model <- randomForest(SpeciesID ~ ., data = train_data, ntree = 3000)
    
    preds_group <- predict(rf_group_model, newdata = test_data)
    
    cm_group <- confusionMatrix(preds_group, test_data$SpeciesID)
    
    precision <- cm_group$byClass[, "Precision"]
    recall <- cm_group$byClass[, "Recall"]
    f1_by_class <- 2 * (precision * recall) / (precision + recall)
    f1_macro <- mean(f1_by_class, na.rm = TRUE)
    
    group_iteration_results[[group_name]] <- list(
      confusion = cm_group,
      accuracy = cm_group$overall["Accuracy"],
      f1_by_class = f1_by_class,
      f1_macro = f1_macro
    )
  }
  
  grouped_accuracy_results[[paste0("Sample_", i)]] <- group_iteration_results
}

beep()

saveRDS(all_results, "E:/Results/Balanaced_Full_RF_results_all_metrics.rds")
saveRDS(importance_results, "E:/Results/Balanced_Full_RF_importance_all_metrics.rds")
saveRDS(grouped_accuracy_results, "E:/Results/Balanced_Grouped_RF_accuracy_only.rds")


