

library(dplyr)
library(caret)
library(tidyr)
library(stringr)
library(beepr)
beep()


# Step 1–2: Your preprocessing steps remain the same
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean_sunlit_no_nas.csv")
str(spec_chem_canopy)

# Pick the single metric to use 
metric_to_use <- c("Boochs2", "PAD_30_35_off", "SR7", "NPCI", "PAD_30_35_on", 
                   "Carter5", "PAD_35_40_on", "MCARI2OSAVI2", "SRPI", "PSRI",
                   "PAD_10_15_on", "MCARIOSAVI", "MCARI2", "PAD_25_30_on", "mNDVI",
                   "PRICI2", "SIPI", "Datt5", "PAD_35_40_off", "OSAVI2",
                   "Carter", "PAD_20_25_off", "PAD_10_15_off", "SR5", "SPVI",
                   "PAD_25_30_off", "Carter2", "CARI", "Gitelson2", "NDVI3",
                   "TGI", "DD", "PSND", "DWSI4", "PAD_15_20_off", "MCARI")

# Summarize multiple metrics per canopy
canopy_summary <- spec_chem_canopy %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metric_to_use), mean, na.rm = TRUE), .groups = "drop")

# Identify species with >8 canopies for training
species_for_training <- canopy_summary %>%
  count(SpeciesID) %>%
  filter(n > 5) %>%
  pull(SpeciesID)

# Prepare list for results
iter_results <- list()

for (i in 1:50) {
  set.seed(50 + i)
  cat(sprintf("\nIteration %d\n", i))
  
  # Training canopies: 70% stratified per species (only from species_for_training)
  train_canopies <- canopy_summary %>%
    filter(SpeciesID %in% species_for_training) %>%
    group_by(SpeciesID) %>%
    slice_sample(prop = 0.7) %>%
    ungroup()
  
  # Test canopies: all canopies NOT in training (including species not in training)
  test_canopies <- canopy_summary %>%
    filter(!TreeID %in% train_canopies$TreeID)
  
  # Prepare training and testing datasets
  train_data <- train_canopies %>%
    select(SpeciesID, all_of(metric_to_use))
  
  test_data <- test_canopies %>%
    select(SpeciesID, all_of(metric_to_use))
  
  # Ensure factor levels are consistent
  train_data$SpeciesID <- as.factor(train_data$SpeciesID)
  test_data$SpeciesID <- factor(test_data$SpeciesID, levels = levels(train_data$SpeciesID))
  
  # Train random forest
  rf_mod <- ranger::ranger(
    SpeciesID ~ .,
    data = train_data,
    num.trees = 1000,
    probability = TRUE
  )
  
  # Predict on test set
  rf_pred_prob <- predict(rf_mod, data = test_data)
  
  predicted_probabilities <- rf_pred_prob$predictions
  predicted_class_index <- apply(predicted_probabilities, 1, which.max)
  predicted_class <- colnames(predicted_probabilities)[predicted_class_index]
  confidence_values <- apply(predicted_probabilities, 1, max)
  
  # Build results dataframe
  results <- data.frame(
    TreeID = test_canopies$TreeID,
    Actual_Class = test_canopies$SpeciesID,
    Predicted_Class = predicted_class,
    Confidence = confidence_values
  )
  
  # Accuracy
  accuracy <- mean(results$Predicted_Class == results$Actual_Class)
  
  # Confusion matrix
  cm <- caret::confusionMatrix(
    factor(results$Predicted_Class, levels = levels(train_data$SpeciesID)),
    factor(results$Actual_Class, levels = levels(train_data$SpeciesID))
  )
  
  # Per-class F1
  per_class_stats <- cm$byClass
  if (is.matrix(per_class_stats)) {
    f1_scores <- per_class_stats[, "F1"]
  } else {
    f1_scores <- setNames(per_class_stats["F1"], levels(train_data$SpeciesID))
  }
  macro_f1 <- mean(f1_scores, na.rm = TRUE)
  
  cat("Iteration accuracy:", accuracy, "\n")
  cat("Iteration macro F1 score:", macro_f1, "\n")
  
  iter_results[[i]] <- list(
    results = results,
    accuracy = accuracy,
    macro_f1 = macro_f1,
    f1_per_class = f1_scores,
    confidence = confidence_values
  )
}
beep()

saveRDS(iter_results, file = "E:/Results/Final_Model.rds")
