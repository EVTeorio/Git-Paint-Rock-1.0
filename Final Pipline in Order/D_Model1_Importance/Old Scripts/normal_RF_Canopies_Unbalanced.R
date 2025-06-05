


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

# Get canopy and species info
canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

# Calculate species counts from all canopies (for training filter)
species_counts <- canopies %>%
  count(SpeciesID)

# Species with >8 canopies for training only
species_for_training <- species_counts %>%
  filter(n > 5) %>%
  pull(SpeciesID)

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
  "PAD_30_35_off", "PAD_35_40_off", "PAD_5_10_off",
  "PAD_0_5_on", "PAD_10_15_on", "PAD_15_20_on", "PAD_20_25_on", "PAD_25_30_on",
  "PAD_30_35_on", "PAD_35_40_on","PAD_5_10_on",
  "Seasonal_Occupancy_20_35m"
)

# Precompute canopy-averaged data for all canopies (all species included)
canopy_means <- spec_chem_canopy %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE), .groups = "drop")

all_results <- list()
importance_results <- list()

for (i in 1:20) {
  cat("\n--- Object-Based Sample", i, "---\n")
  set.seed(100 + i)
  
  # Training canopies sampled only from species with >8 canopies
  train_canopies <- canopy_means %>%
    filter(SpeciesID %in% species_for_training) %>%
    group_by(SpeciesID) %>%
    slice_sample(prop = 0.7) %>%
    ungroup()
  
  # Testing canopies are all other canopies excluding training canopies (all species)
  test_canopies <- canopy_means %>%
    filter(!TreeID %in% train_canopies$TreeID) %>%
    ungroup()
  
  train_canopies$SpeciesID <- as.factor(train_canopies$SpeciesID)
  test_canopies$SpeciesID <- as.factor(test_canopies$SpeciesID)
  
  # Prepare data frames for model
  train_data <- train_canopies %>%
    select(SpeciesID, all_of(metrics))
  
  test_data <- test_canopies %>%
    select(SpeciesID, all_of(metrics))
  
  # Train Random Forest model
  rf_model <- randomForest(SpeciesID ~ ., data = train_data, ntree = 3000, importance = TRUE)
  
  # Predict on test data
  preds <- predict(rf_model, newdata = test_data)
  
  # Evaluate performance
  cm <- confusionMatrix(preds, test_data$SpeciesID)
  
  precision <- cm$byClass[, "Precision"]
  recall <- cm$byClass[, "Recall"]
  f1_by_class <- 2 * (precision * recall) / (precision + recall)
  f1_macro <- mean(f1_by_class, na.rm = TRUE)
  
  # Store results
  all_results[[paste0("Sample_", i)]] <- list(
    model = rf_model,
    confusion = cm,
    accuracy = cm$overall["Accuracy"],
    f1_by_class = f1_by_class,
    f1_macro = f1_macro
  )
  
  importance_results[[paste0("Sample_", i)]] <- importance(rf_model)
}

beep(3)
saveRDS(species_importance_results, "E:/Results/Unbalanced_object_based_results.rds")

#######################################################################3

summary_list <- list()

for (sample_name in names(all_results)) {
  res <- all_results[[sample_name]]
  cm <- res$confusion
  
  # cm$byClass rows correspond to species in order (usually alphabetical)
  species_names <- rownames(cm$byClass)
  
  # For each species, extract metrics
  for (sp in species_names) {
    precision <- cm$byClass[sp, "Precision"]
    recall <- cm$byClass[sp, "Recall"]
    f1 <- 2 * (precision * recall) / (precision + recall)
    accuracy <- as.numeric(res$accuracy)  # overall accuracy
    
    row <- data.frame(
      Sample = sample_name,
      Species = sp,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      stringsAsFactors = FALSE
    )
    summary_list[[paste(sample_name, sp, sep = "_")]] <- row
  }
}

species_summary_df <- bind_rows(summary_list)

write.csv(species_summary_df,
          "E:/Results/object_based_model_summary_per_species.csv",
          row.names = FALSE)

###plot accuracies
# Remove "Class: " prefix from species names to match with species_for_training
species_summary_df <- species_summary_df %>%
  mutate(Species_clean = gsub("Class: ", "", Species))

# Filter to only species used in training (12 species)
filtered_df <- species_summary_df %>%
  filter(Species_clean %in% species_for_training)

# --- F1 Score Boxplot ---
ggplot(filtered_df, aes(x = Species_clean, y = F1)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "F1 Score Distribution by Species",
       x = "Species",
       y = "F1 Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("E:/Results/f1_score_boxplot_12_species.png", width = 10, height = 6)

##############################################################################3

# Process and clean importance results
species_importance_df <- map_dfr(
  .x = names(importance_results),
  .f = function(sample_name) {
    imp_matrix <- importance_results[[sample_name]]
    imp_df <- as.data.frame(imp_matrix)
    imp_df$Feature <- rownames(imp_df)
    imp_df$Sample <- sample_name
    # Convert to long format
    long_df <- pivot_longer(imp_df, 
                            cols = -c(Feature, Sample), 
                            names_to = "SpeciesID", 
                            values_to = "ImportanceValue")
    # Filter out MeanDecreaseAccuracy and MeanDecreaseGini
    long_df %>% 
      filter(!SpeciesID %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))
  }
)

# Save to CSV (optional)
write.csv(species_importance_df, "E:/Results/species_feature_importance_by_sample.csv", row.names = FALSE)

# Preview the data
print(species_importance_df, n = 100)


#################################################################
# Create output folder if needed
output_dir <- "E:/Git Paint Rock 1.0/Output/Analysis/Feature_Boxplots_CanopyBased/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get unique species
all_species <- unique(species_importance_df$SpeciesID)

# Loop through each species
for (species_to_plot in all_species) {
  
  # Filter importance data for the current species
  df_plot <- species_importance_df %>%
    filter(SpeciesID == species_to_plot)
  
  # Get top 15 features by average absolute importance value
  top_features <- df_plot %>%
    group_by(Feature) %>%
    summarise(MeanAbsImportance = mean(abs(ImportanceValue), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(MeanAbsImportance)) %>%
    slice_head(n = 15) %>%
    pull(Feature)
  
  # Filter to only top 15 features
  df_plot <- df_plot %>%
    filter(Feature %in% top_features)
  
  # Count sample occurrences per feature (for labeling)
  feature_counts <- df_plot %>%
    group_by(Feature) %>%
    summarise(Count = n(), .groups = "drop")
  
  # Order features by average importance for nicer plot
  feature_order <- df_plot %>%
    group_by(Feature) %>%
    summarise(MeanImportance = mean(ImportanceValue, na.rm = TRUE)) %>%
    arrange(MeanImportance) %>%
    pull(Feature)
  
  df_plot$Feature <- factor(df_plot$Feature, levels = feature_order)
  
  # Plot boxplot
  p <- ggplot(df_plot, aes(x = Feature, y = ImportanceValue)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Top 15 Feature Importances for", species_to_plot),
      x = "Feature",
      y = "Importance Value"
    ) +
    scale_x_discrete(labels = function(x) {
      counts <- feature_counts$Count[match(x, feature_counts$Feature)]
      paste0(x, " (n=", counts, ")")
    })
  
  # Save plot
  ggsave(
    filename = file.path(output_dir, paste0("Feature_Importance_", species_to_plot, ".png")),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
}
###################################################################


# 1. Calculate mean importance per feature per species (only if present in >=4 samples)
species_feature_means <- species_importance_df %>%
  group_by(SpeciesID, Feature) %>%
  summarise(MeanImportance = mean(ImportanceValue, na.rm = TRUE), .groups = "drop")

# 2. Initialize empty vector for selected features
selected_features <- c()

# 3. Get list of unique species
species_list <- unique(species_feature_means$SpeciesID)

# 4. Iteratively select top feature per species (skipping already selected ones)
while (length(selected_features) < 36) {
  for (sp in species_list) {
    sp_feats <- species_feature_means %>%
      filter(SpeciesID == sp, !(Feature %in% selected_features)) %>%
      arrange(desc(MeanImportance))
    
    if (nrow(sp_feats) > 0) {
      next_best_feat <- sp_feats$Feature[1]
      selected_features <- unique(c(selected_features, next_best_feat))
    }
    
    if (length(selected_features) >= 36) break
  }
}

# 5. Construct summary table: rows = selected features, columns = species
final_summary_df <- species_feature_means %>%
  filter(Feature %in% selected_features) %>%
  pivot_wider(names_from = SpeciesID, values_from = MeanImportance) %>%
  arrange(match(Feature, selected_features))  # Maintain feature selection order

# 6. Save the summary to CSV
write.csv(final_summary_df,
          "E:/Thesis_Final_Data/Analysis/Top36_Feature_Species_MeanImportance_object_based.csv",
          row.names = FALSE)
#################################################################################

library(tidyverse)

# 1. Convert wide to long format
tree_feature_means <- final_summary_df %>%
  pivot_longer(-Feature, names_to = "Species", values_to = "MeanImportance")

# 2. Order features by overall mean importance
feature_order <- tree_feature_means %>%
  group_by(Feature) %>%
  summarise(OverallMean = mean(MeanImportance, na.rm = TRUE), .groups = "drop") %>%
  arrange(OverallMean) %>%
  pull(Feature)

tree_feature_means$Feature <- factor(tree_feature_means$Feature, levels = feature_order)

# 3. Plot
ggplot(tree_feature_means, aes(x = MeanImportance, y = Feature)) +
  geom_point(aes(color = Species, size = MeanImportance), alpha = 0.85) +
  scale_size_continuous(range = c(2, 8), guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_brewer(palette = "Paired") +  # You can also try "Dark2", "Set1", or use scale_color_manual
  labs(
    title = "Top 36 Features by Species Mean Importance",
    x = "Mean Decrease Gini (averaged across samples)",
    y = "Feature (ranked by overall importance)",
    size = "Importance",
    color = "Species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    legend.key.size = unit(1.5, "lines")  # Increase legend dot size
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 6)),  # Larger color dots in legend
    size = guide_legend(override.aes = list(shape = 16))
  )
print(selected_features)

