

\library(dplyr)
library(ranger)
library(caret)
library(beepr)

# Load data
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean_sunlit_no_nas.csv")

# Define metric groups
vi_vars <- c(
 "Carter5", "Datt5", "mNDVI", "NPCI", "OSAVI", "PARS", "PSRI", "SIPI","SR7"
)

leafon_vars <- c("PAD_20_25_on", "PAD_25_30_on", "PAD_30_35_on", "PAD_35_40_on")

leafoff_vars <- c("PAD_20_25_off", "PAD_25_30_off", "PAD_30_35_off", "PAD_35_40_off")

seasonal_var <- "Seasonal_Occupancy_20_35m"

# Groupings
model_inputs <- list(
  VIs_only = vi_vars,
  VIs_LiDARleafon = c(vi_vars, leafon_vars),
  VIs_LiDARleafoff = c(vi_vars, leafoff_vars),
  VIs_allLiDAR = c(vi_vars, leafon_vars, leafoff_vars, seasonal_var)
)

# Average metrics by canopy for ALL variables
canopy_means <- spec_chem_canopy %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")

Final_grouped_results < list()

for (group_name in names(model_inputs)) {
  cat("\n============== Modeling Group:", group_name, "===============\n")
  
  metrics_in_use <- model_inputs[[group_name]]
  group_results <- list()
  
  for (i in 1:50) {
    cat("\n--- Iteration", i, "---\n")
    set.seed(50 + i)
    
    # Filter species with ≥23 canopies
    species_counts <- canopy_means %>%
      count(SpeciesID)
    
    eligible_species <- species_counts %>%
      filter(n >= 18) %>%
      pull(SpeciesID)
    
    # Sample canopies from each eligible species
    sampled_canopies <- canopy_means %>%
      filter(SpeciesID %in% eligible_species) %>%
      group_by(SpeciesID) %>%
      slice_sample(n = 18) %>%
      ungroup()
    
    # Training per species
    train_canopies <- sampled_canopies %>%
      group_by(SpeciesID) %>%
      slice_sample(n = 10) %>%
      ungroup()
    
    # Test: remaining per eligible species
    sampled_test_canopies <- sampled_canopies %>%
      filter(!TreeID %in% train_canopies$TreeID)
    
    # Test: all canopies from species NOT eligible
    rare_species_test_canopies <- canopy_means %>%
      filter(!(SpeciesID %in% eligible_species))
    
    # Combine test sets
    test_canopies <- bind_rows(sampled_test_canopies, rare_species_test_canopies)
    
    # Prepare training/testing data
    train_data <- train_canopies %>%
      select(SpeciesID, all_of(metrics_in_use)) %>%
      drop_na()
    
    test_data <- test_canopies %>%
      select(SpeciesID, all_of(metrics_in_use)) %>%
      drop_na()
    
    # Match factor levels
    train_data$SpeciesID <- as.factor(train_data$SpeciesID)
    test_data$SpeciesID <- factor(test_data$SpeciesID, levels = levels(train_data$SpeciesID))
    
    # Train random forest
    rf_mod <- ranger(
      SpeciesID ~ .,
      data = train_data,
      num.trees = 1000,
      probability = TRUE
    )
    
    # Predict
    rf_pred <- predict(rf_mod, data = test_data)
    pred_probs <- rf_pred$predictions
    pred_class_index <- apply(pred_probs, 1, which.max)
    pred_class <- colnames(pred_probs)[pred_class_index]
    confidence <- apply(pred_probs, 1, max)
    
    # Evaluation
    results_df <- data.frame(
      TreeID = test_canopies$TreeID,
      Actual_Class = test_canopies$SpeciesID,
      Predicted_Class = pred_class,
      Confidence = confidence
    )
    
    # Only evaluate accuracy/F1 on species present in training
    filtered_results <- results_df %>%
      filter(Actual_Class %in% levels(train_data$SpeciesID))
    
    accuracy <- mean(filtered_results$Predicted_Class == filtered_results$Actual_Class)
    
    cm <- confusionMatrix(
      factor(filtered_results$Predicted_Class, levels = levels(train_data$SpeciesID)),
      factor(filtered_results$Actual_Class, levels = levels(train_data$SpeciesID))
    )
    
    per_class_stats <- cm$byClass
    f1_scores <- if (is.matrix(per_class_stats)) {
      per_class_stats[, "F1"]
    } else {
      setNames(per_class_stats["F1"], levels(train_data$SpeciesID))
    }
    macro_f1 <- mean(f1_scores, na.rm = TRUE)
    
    cat("Accuracy:", round(accuracy, 4), "\n")
    cat("Macro F1:", round(macro_f1, 4), "\n")
    
    group_results[[paste0("Iter_", i)]] <- list(
      model = rf_mod,
      accuracy = accuracy,
      macro_f1 = macro_f1,
      f1_per_class = f1_scores,
      results = results_df,
      confidence = confidence,
      confusion_matrix = cm
    )
  }
  
  Final_grouped_results[[group_name]] <- group_results
}

beep()

saveRDS(Final_grouped_results, file = "E:/Results/Final_Model.rds")

