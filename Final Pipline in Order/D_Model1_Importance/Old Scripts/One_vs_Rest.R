

library(dplyr)
library(stringr)
library(randomForest)
library(caret)
library(beepr)

# --- Load data ---
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean_sunlit_no_nas.csv")
beep()
str(spec_chem_canopy)

# --- Extract unique canopies ---
canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

# --- Keep species with >5 canopies ---
species_counts <- canopies %>%
  count(SpeciesID) %>%
  filter(n > 5)

canopies_filtered <- canopies %>%
  filter(SpeciesID %in% species_counts$SpeciesID)

# --- Metrics list ---
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

# --- Initialize result storage ---
species_importance_results <- list()

# --- Loop over 10 sampling iterations ---
for (i in 1:10) {
  cat("\n--- Running Sample", i, "---\n")
  set.seed(100 + i)
  
  # Sample 4 training and 2 test canopies per species
  train_canopies <- canopies_filtered %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 4) %>%
    ungroup()
  
  test_canopies <- anti_join(canopies_filtered, train_canopies, by = "TreeID") %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 2) %>%
    ungroup()
  
  # Get training/test pixel data
  train_df <- spec_chem_canopy %>%
    filter(TreeID %in% train_canopies$TreeID)
  
  #if more than 4 canopies selected set to number of samples in training data
  # train_df <- train_pixels %>%
  #   group_by(SpeciesID) %>%
  #   slice_sample(n = 800) %>%
  #   ungroup()
  
  test_df <- spec_chem_canopy %>%
    filter(TreeID %in% test_canopies$TreeID) %>%
    group_by(TreeID) %>%
    slice_sample(n = 300) %>%
    ungroup()
  
  # Make sure species are factors
  train_df$SpeciesID <- as.factor(train_df$SpeciesID)
  test_df$SpeciesID <- as.factor(test_df$SpeciesID)
  
  # Balance training data 
  balanced_train_df <- train_df %>%
    group_by(TreeID) %>%
    slice_sample(n = 150) %>%
    ungroup()
  
  # One-vs-rest binary classification
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
    
    # Skip if not enough classes
    if (length(unique(train_binary$binary_label)) < 2 ||
        length(unique(test_binary$binary_label)) < 2) {
      warning(paste("Not enough data for species", species, "in sample", i))
      next
    }
    
    # Train binary random forest
    rf_bin <- randomForest(binary_label ~ ., data = train_binary, ntree = 1000, importance = TRUE)
    pred_bin <- predict(rf_bin, test_binary)
    cm_bin <- confusionMatrix(pred_bin, test_binary$binary_label, positive = "target")
    
    # Evaluation
    acc_bin <- cm_bin$overall["Accuracy"]
    precision_bin <- cm_bin$byClass["Precision"]
    recall_bin <- cm_bin$byClass["Recall"]
    f1_bin <- cm_bin$byClass["F1"]
    
    # Variable importance
    imp_bin <- importance(rf_bin)[, "MeanDecreaseGini", drop = FALSE]
    imp_bin <- sort(imp_bin[, 1], decreasing = TRUE)
    top_features <- head(imp_bin, 15)
    
    binary_results[[as.character(species)]] <- list(
      accuracy = acc_bin,
      precision = precision_bin,
      recall = recall_bin,
      f1 = f1_bin,
      top_features = top_features
    )
  }
  
  species_importance_results[[paste0("Sample_", i)]] <- binary_results
}
beep()
# Optional: save the results
saveRDS(species_importance_results, "E:/Thesis_Final_Data/12species_importance_binary.rds")



##########################################################################################
species_importance_results <- readRDS("E:/Thesis_Final_Data/12species_importance_binary.rds")
beep()

# Initialize list to collect rows
summary_list <- list()

for (sample_name in names(species_importance_results)) {
  sample_results <- species_importance_results[[sample_name]]
  
  for (species in names(sample_results)) {
    res <- sample_results[[species]]
    
    # Create a one-row data frame with Sample, Species, Accuracy, Precision, Recall, F1
    row <- data.frame(
      Sample = sample_name,
      Species = species,
      Accuracy = as.numeric(res$accuracy),
      Precision = as.numeric(res$precision),
      Recall = as.numeric(res$recall),
      F1 = as.numeric(res$f1),
      stringsAsFactors = FALSE
    )
    
    summary_list[[paste(sample_name, species, sep = "_")]] <- row
  }
}

# Combine all rows into one data frame
species_summary_df <- bind_rows(summary_list)

# Save to CSV
write.csv(species_summary_df,
          "E:/Thesis_Final_Data/Analysis/12_species_binary_model_summary.csv", row.names = FALSE)
############################################################################################

# Initialize list to collect importance data
importance_list <- list()

for (sample_name in names(species_importance_results)) {
  sample_results <- species_importance_results[[sample_name]]
  
  for (species in names(sample_results)) {
    imp_vec <- sample_results[[species]]$top_features
    
    if (!is.null(imp_vec)) {
      # Create a data frame with Sample, Species, Feature, Importance
      imp_df <- data.frame(
        Sample = sample_name,
        Species = species,
        Feature = names(imp_vec),
        Importance = as.numeric(imp_vec),
        stringsAsFactors = FALSE
      )
      importance_list[[paste(sample_name, species, sep = "_")]] <- imp_df
    }
  }
}

# Combine into one data frame for plotting
importance_df <- bind_rows(importance_list)

write.csv(importance_df, "E:/Thesis_Final_Data/Analysis/12species_feature_importance_long.csv", row.names = FALSE)


# Create output folder if needed
output_dir <- "E:/Git Paint Rock 1.0/Output/Analysis/Feature_Boxplots/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get unique species from importance data
all_species <- unique(importance_df$Species)

# Loop through each species
for (species_to_plot in all_species) {
  
  # Filter for species
  df_plot <- importance_df %>%
    filter(Species == species_to_plot)
  
  # Count occurrences of each feature in the top 10
  feature_counts <- df_plot %>%
    group_by(Feature) %>%
    summarise(Count = n()) %>%
    arrange(Count)  # Ascending order
  
  # Join back to add count info
  df_plot <- df_plot %>%
    left_join(feature_counts, by = "Feature")
  
  # Order features by ascending count
  df_plot$Feature <- factor(df_plot$Feature, levels = feature_counts$Feature)
  
  # Create plot
  p <- ggplot(df_plot, aes(x = Feature, y = Importance)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Feature Importance for", species_to_plot),
      x = "Feature (Top 10 Count)",
      y = "Mean Decrease Gini"
    ) +
    scale_x_discrete(labels = function(x) {
      counts <- feature_counts$Count[match(x, feature_counts$Feature)]
      paste0(x, " (n=", counts, ")")
    })
  
  # Save plot
  ggsave(
    filename = paste0(output_dir, "12Species_Feature_Importance_binary", species_to_plot, ".png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # Print message
  message("Saved plot for species: ", species_to_plot)
}

##############################################################################


# 1. Calculate mean importance per feature per species (only if present in >=4 samples)
species_feature_means <- importance_df %>%
  group_by(Species, Feature) %>%
  filter(n() >= 4) %>%  # Ensure feature appears in at least 4 samples
  summarise(MeanImportance = mean(Importance), .groups = "drop")

# 2. Initialize container for selected features
selected_features <- c()

# 3. Track species-wise top features (excluding already selected ones)
species_list <- unique(species_feature_means$Species)

while (length(selected_features) < 24) {
  for (sp in species_list) {
    # Filter and sort features for this species (excluding already selected ones)
    sp_feats <- species_feature_means %>%
      filter(Species == sp & !(Feature %in% selected_features)) %>%
      arrange(desc(MeanImportance))
    
    if (nrow(sp_feats) > 0) {
      next_best_feat <- sp_feats$Feature[1]
      selected_features <- unique(c(selected_features, next_best_feat))
    }
    
    if (length(selected_features) >= 24) break
  }
}

# 4. Build final summary table: each feature × species mean importance
final_summary_df <- species_feature_means %>%
  filter(Feature %in% selected_features) %>%
  tidyr::pivot_wider(names_from = Species, values_from = MeanImportance) %>%
  arrange(match(Feature, selected_features))  # Preserve feature selection order

# Save to CSV
write.csv(final_summary_df, "E:/Thesis_Final_Data/Analysis/Top24_Feature_Species_MeanImportance_binary.csv", row.names = FALSE)

# 1. Get selected features
selected_features <- final_summary_df$Feature

# 2. Filter the original per-sample importance_df for only these features
per_tree_imp_df <- importance_df %>%
  filter(Feature %in% selected_features)

# 3. Compute mean importance per (Species, Sample, Feature)
tree_feature_means <- per_tree_imp_df %>%
  group_by(Species, Sample, Feature) %>%
  summarise(MeanImportance = mean(Importance), .groups = "drop")

# 4. Compute global mean importance per feature to determine feature order
feature_ranking <- tree_feature_means %>%
  group_by(Feature) %>%
  summarise(GlobalMeanImportance = mean(MeanImportance), .groups = "drop") %>%
  arrange(GlobalMeanImportance)

# 5. Set factor levels of Feature in tree_feature_means according to ranking
tree_feature_means$Feature <- factor(tree_feature_means$Feature, levels = feature_ranking$Feature)

# 6. Plot with features ordered by importance on y-axis
ggplot(tree_feature_means, aes(x = MeanImportance, y = Feature)) +
  geom_point(aes(color = Species, size = MeanImportance), alpha = 0.8) +
  scale_size_continuous(range = c(2, 8)) +
  labs(
    title = "Top 24 Features by Species Mean Importance",
    x = "Mean Decrease Gini (per sample)",
    y = "Feature (ranked by overall importance)",
    size = "Importance"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "right"
  )

#########Output Seleeted features########################
# Assuming `feature_ranking` contains the ranked features
top_features <- feature_ranking$Feature
print(top_features)

