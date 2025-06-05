

# Load libraries
library(dplyr)
library(stringr)
library(caret)
library(randomForest)
library(beepr)
library(tidyr)

# Read in full dataset
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean_sunlit_no_nas.csv")
beep()
str(spec_chem_canopy)
# Extract unique canopies
canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

# Keep only species with >8 canopies
species_counts <- canopies %>%
  count(SpeciesID) %>%
  filter(n > 5)

canopies_filtered <- canopies %>%
  filter(SpeciesID %in% species_counts$SpeciesID)

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
  "Vogelmann", "Vogelmann2", "Vogelmann3", "Vogelmann4",
)

# Identify LiDAR variables
leafon_vars <- c("PAD_0_5_on", "PAD_10_15_on", "PAD_15_20_on", "PAD_20_25_on", "PAD_25_30_on",
                 "PAD_30_35_on", "PAD_35_40_on","PAD_5_10_on")

leafoff_vars <- c("PAD_0_5_off", "PAD_10_15_off", "PAD_15_20_off", "PAD_20_25_off", "PAD_25_30_off",
                  "PAD_30_35_off", "PAD_35_40_off", "PAD_5_10_off")

seasonal_var <- "Seasonal_Occupancy_20_35m"

# Define model inputs
model_inputs <- list(
  VIs_only = vi_vars,
  VIs_LiDARleafon = c(vi_vars, leafon_vars),
  VIs_LiDARleafoff = c(vi_vars, leafoff_vars),
  VIs_allLiDAR = c(vi_vars, leafon_vars, leafoff_vars, seasonal_var)
)
# Store results from 10 repetitions
all_results <- list()
importance_results <- list()  # New list to store variable importance

for (i in 1:10) {
  cat("\n--- Running Sample", i, "---\n")
  set.seed(100 + i)
  
  # Select 5 canopies per species for training
  train_canopies <- canopies_filtered %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 4) %>%
    ungroup()
  
  test_canopies_all <- anti_join(canopies_filtered, train_canopies, by = "TreeID")
  
  # Sample exactly 2 test canopies randomly from remaining
  test_canopies <- test_canopies_all %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 2) %>%
    ungroup()
  
  # Get training and test pixels
  train_df <- spec_chem_canopy %>%
    filter(TreeID %in% train_canopies$TreeID)
  
  # For testing: sample 200 pixels per test canopy (balanced)
  test_df <- spec_chem_canopy %>%
    filter(TreeID %in% test_canopies$TreeID) %>%
    group_by(TreeID) %>%
    slice_sample(n = 200) %>%
    ungroup()
  
  # Ensure factors
  train_df$SpeciesID <- as.factor(train_df$SpeciesID)
  test_df$SpeciesID <- as.factor(test_df$SpeciesID)
  
  # Sample 50 pixels per training TreeID
  balanced_train_df <- train_df %>%
    group_by(TreeID) %>%
    slice_sample(n = 50) %>%
    ungroup()
  
  # Store results per model type
  results <- list()
  importances <- list()  # Temp list to hold importance for each model in this iteration
  
  for (model_name in names(model_inputs)) {
    cat("  -> Training model:", model_name, "\n")
    
    model_vars <- model_inputs[[model_name]]
    
    # Select and clean training data
    train_data <- balanced_train_df %>%
      select(SpeciesID, all_of(model_vars))
      
    
    test_data <- test_df %>%
      select(SpeciesID, all_of(model_vars))
    
    # Train model
    rf_model <- randomForest(SpeciesID ~ ., data = train_data, ntree = 3000, predicted = TRUE)
    
    importance_model <- randomForest(SpeciesID ~ ., data = train_data, ntree = 3000, importance = TRUE)
    
    # Predict on independent test canopy pixels
    preds <- predict(rf_model, newdata = test_data)
    
    # Evaluate
    cm <- confusionMatrix(preds, test_data$SpeciesID)
    
    precision <- cm$byClass[, "Precision"]
    recall <- cm$byClass[, "Recall"]
    f1_by_class <- 2 * (precision * recall) / (precision + recall)
    f1_macro <- mean(f1_by_class, na.rm = TRUE)
    
    results[[model_name]] <- list(
      model = rf_model,
      confusion = cm,
      accuracy = cm$overall["Accuracy"],
      f1_by_class = f1_by_class,
      f1_macro = f1_macro
    )
    
    # Extract and store variable importance
    importances[[model_name]] <- importance(importance_model)
  }
  
  all_results[[paste0("Sample_", i)]] <- results
  importance_results[[paste0("Sample_", i)]] <- importances  # Save importance per model per sample
}
beep(3)

# Save both results and importances
saveRDS(all_results, file = "E:/Thesis_Final_Data/12species_all_rf_results.rds")
saveRDS(importance_results, file = "E:/Thesis_Final_Data/12species_all_rf_importances.rds")

beep()
# To load later
# all_result <- readRDS("E:/Thesis_Final_Data/all_rf_results.rds")
########################################################################

# Initialize list to collect rows
summary_list <- list()

for (sample_name in names(all_results)) {
  sample_results <- all_results[[sample_name]]
  
  for (model_name in names(sample_results)) {
    result <- sample_results[[model_name]]
    
    # Rename class F1 scores to valid column names
    class_f1 <- setNames(as.numeric(result$f1_by_class), 
                         paste0("F1_", gsub("Class: ", "", names(result$f1_by_class))))
    
    # One row with overall metrics + per-class F1 scores
    row <- c(
      Sample = sample_name,
      Model = model_name,
      Accuracy = result$accuracy,
      F1_Macro = result$f1_macro,
      class_f1
    )
    
    summary_list[[paste(sample_name, model_name, sep = "_")]] <- as.data.frame(t(row), stringsAsFactors = FALSE)
  }
}

# Combine into one data frame and rename to summary_metrics
summary_metrics <- bind_rows(summary_list)

# Convert numeric columns appropriately
numeric_cols <- setdiff(names(summary_metrics), c("Sample", "Model"))
summary_metrics[numeric_cols] <- lapply(summary_metrics[numeric_cols], as.numeric)

# Save to CSV
write.csv(summary_metrics, "E:/Thesis_Final_Data/Analysis/12_species_model_summary.csv", row.names = FALSE)
##############################################################################

# Accuracy boxplot across models
ggplot(summary_metrics %>% distinct(Sample, Model, Accuracy.Accuracy), 
       aes(x = Model, y = Accuracy.Accuracy)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Accuracy by Model Across 10 Samples",
       x = "Model Type", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# F1 per class, grouped by model
f1_long <- summary_metrics %>%
  pivot_longer(cols = starts_with("F1_") & !starts_with("F1_Macro"), 
               names_to = "Class", values_to = "F1") %>%
  filter(!is.na(F1))

ggplot(f1_long, aes(x = Class, y = F1, fill = Model)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "F1 Score by Class and Model", x = "Class", y = "F1 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################

output_dir <- "E:/Thesis_Final_Data/Analysis/ConfusionMatrixes"

for (sample_name in names(all_results)) {
  for (model_name in names(all_results[[sample_name]])) {
    result <- all_results[[sample_name]][[model_name]]
    
    # Confusion matrix
    cm_file <- file.path(output_dir, paste0(sample_name, "_", model_name, "12species_confusion.csv"))
    write.csv(as.table(result$confusion$table), cm_file)
  }
}
#################################################################################
###########Importance Summary
# Initialize list to collect importance rows
importance_list <- list()

for (sample_name in names(importance_results)) {
  sample_importances <- importance_results[[sample_name]]
  
  for (model_name in names(sample_importances)) {
    imp_df <- as.data.frame(sample_importances[[model_name]])
    imp_df$Variable <- rownames(imp_df)
    imp_df$Sample <- sample_name
    imp_df$Model <- model_name
    
    importance_list[[paste(sample_name, model_name, sep = "_")]] <- imp_df
  }
}

# Combine into one long dataframe
importance_df <- bind_rows(importance_list)

# Clean up and reorder columns
importance_df <- importance_df %>%
  relocate(Sample, Model, Variable)

# Optionally convert columns to appropriate types
importance_df$MeanDecreaseGini <- as.numeric(importance_df$MeanDecreaseGini)

# Save to CSV
write.csv(importance_df, "E:/Thesis_Final_Data/Analysis/12species_importance.csv", row.names = FALSE)

####################################################################################

# Calculate average importance by variable and model
avg_importance <- importance_df %>%
  group_by(Model, Variable) %>%
  summarize(MeanImportance = mean(MeanDecreaseGini, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(MeanImportance))

# Plot top 20 important variables per model
top_vars <- avg_importance %>%
  group_by(Model) %>%
  slice_max(order_by = MeanImportance, n = 20)

ggplot(top_vars, aes(x = reorder(Variable, MeanImportance), y = MeanImportance, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  facet_wrap(~Model, scales = "free_y") +
  labs(title = "Top 20 Variables by Mean Decrease Gini",
       x = "Variable", y = "Mean Decrease Gini") +
  theme_minimal()
#######################################################




#######################################################################

