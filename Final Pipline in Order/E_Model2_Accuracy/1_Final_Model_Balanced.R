

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

Final_grouped_results <- list()

for (group_name in names(model_inputs)) {
  cat("\n============== Modeling Group:", group_name, "===============\n")
  
  metrics_in_use <- model_inputs[[group_name]]
  group_results <- list()
  
  for (i in 1:50) {
    cat("\n--- Iteration", i, "---\n")
    set.seed(50 + i)
    
    # Count canopies per species
    species_counts <- canopy_means %>% count(SpeciesID)
    
    species_12_plus <- species_counts %>% filter(n >= 12) %>% pull(SpeciesID)
    species_6_11 <- species_counts %>% filter(n >= 6 & n < 12) %>% pull(SpeciesID)
    rare_species <- species_counts %>% filter(n < 6) %>% pull(SpeciesID)
    
    # Sample species with >=12 canopies
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
    
    # Sample species with 6-11 canopies
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
    
    # Handle rare species: 1 for training, 1 for test; label all as "others"
    rare_canopies <- canopy_means %>%
      filter(SpeciesID %in% rare_species)
    
    rare_train <- rare_canopies %>%
      group_by(SpeciesID) %>%
      slice_sample(n = 1) %>%
      ungroup() %>%
      mutate(SpeciesID = "others")
    
    rare_test <- rare_canopies %>%
      filter(!TreeID %in% rare_train$TreeID) %>%
      group_by(SpeciesID) %>%
      slice_sample(n = 1) %>%
      ungroup() %>%
      mutate(SpeciesID = "others")
    
    # Combine training and test sets
    train_canopies <- bind_rows(train_12_plus, train_6_11, rare_train)
    test_canopies <- bind_rows(test_12_plus, test_6_11, rare_test)
    
    # Prepare training and test data
    train_data <- train_canopies %>%
      select(SpeciesID, all_of(metrics_in_use)) %>%
      drop_na()
    
    test_data <- test_canopies %>%
      select(SpeciesID, all_of(metrics_in_use)) %>%
      drop_na()
    
    train_data$SpeciesID <- factor(train_data$SpeciesID)
    test_data$SpeciesID <- factor(test_data$SpeciesID, levels = levels(train_data$SpeciesID))
    
    # Train the ranger model
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
    
    filtered_results <- results_df %>%
      filter(Actual_Class %in% levels(train_data$SpeciesID))
    
    accuracy <- mean(filtered_results$Predicted_Class == filtered_results$Actual_Class)
    
    cm <- confusionMatrix(
      factor(filtered_results$Predicted_Class, levels = levels(train_data$SpeciesID)),
      factor(filtered_results$Actual_Class, levels = levels(train_data$SpeciesID))
    )
    
    kappa_value <- cm$overall["Kappa"]
    
    per_class_stats <- cm$byClass
    
    f1_scores <- if (is.matrix(per_class_stats)) {
      per_class_stats[, "F1"]
    } else {
      setNames(per_class_stats["F1"], levels(train_data$SpeciesID))
    }
    
    balanced_accuracy_scores <- if (is.matrix(per_class_stats)) {
      per_class_stats[, "Balanced Accuracy"]
    } else {
      setNames(per_class_stats["Balanced Accuracy"], levels(train_data$SpeciesID))
    }
    
    macro_f1 <- mean(f1_scores, na.rm = TRUE)
    macro_balanced_accuracy <- mean(balanced_accuracy_scores, na.rm = TRUE)
    
    cat("Accuracy:", round(accuracy, 4), "\n")
    cat("Kappa:", round(kappa_value, 4), "\n")
    cat("Macro F1:", round(macro_f1, 4), "\n")
    cat("Macro Balanced Accuracy:", round(macro_balanced_accuracy, 4), "\n")
    
    group_results[[paste0("Iter_", i)]] <- list(
      model = rf_mod,
      accuracy = accuracy,
      kappa = as.numeric(kappa_value),
      macro_f1 = macro_f1,
      macro_balanced_accuracy = macro_balanced_accuracy,
      f1_per_class = f1_scores,
      balanced_accuracy_per_class = balanced_accuracy_scores,
      results = results_df,
      confidence = confidence,
      confusion_matrix = cm
    )
  }
  
  Final_grouped_results[[group_name]] <- group_results
}

beep()

saveRDS(Final_grouped_results, file = "E:/Results/Final_Model.rds")

