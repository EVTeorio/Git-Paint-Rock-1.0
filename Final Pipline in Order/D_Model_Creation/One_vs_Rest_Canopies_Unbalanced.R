


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

# Precompute canopy-averaged data
canopy_means <- spec_chem_canopy %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE), .groups = "drop")

# Loop over 10 iterations
species_importance_results <- list()
for (i in 1:10) {
  cat("\n--- Object-Based Sample", i, "---\n")
  set.seed(100 + i)
  
  # For each species, sample 70% canopies for training and the rest for testing
  train_canopies <- canopy_means %>%
    group_by(SpeciesID) %>%
    slice_sample(prop = 0.7) %>%
    ungroup()
  
  test_canopies <- canopy_means %>%
    anti_join(train_canopies, by = "TreeID") %>%
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
saveRDS(species_importance_results, "E:/Thesis_Final_Data/Unbalanced_object_based_results.rds")

################################################################################
# Initialize list to collect summary rows
summary_list <- list()

# Loop through the object-based sampling results
for (sample_name in names(species_importance_results)) {
  sample_results <- species_importance_results[[sample_name]]
  
  for (species in names(sample_results)) {
    res <- sample_results[[species]]
    
    # Safeguard in case of missing metrics
    accuracy <- ifelse(!is.null(res$accuracy), as.numeric(res$accuracy), NA)
    precision <- ifelse(!is.null(res$precision), as.numeric(res$precision), NA)
    recall <- ifelse(!is.null(res$recall), as.numeric(res$recall), NA)
    f1 <- ifelse(!is.null(res$f1), as.numeric(res$f1), NA)
    
    # Create a one-row data frame
    row <- data.frame(
      Sample = sample_name,
      Species = species,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      stringsAsFactors = FALSE
    )
    
    summary_list[[paste(sample_name, species, sep = "_")]] <- row
  }
}

# Combine all rows into one data frame
species_summary_df <- bind_rows(summary_list)

# Save to CSV
write.csv(species_summary_df,
          "E:/Thesis_Final_Data/Analysis/12species_binary_model_summary_object_based_Unbalanced.csv",
          row.names = FALSE)
############################################################################

# Initialize list to collect importance data
importance_list <- list()

# Loop through each sample iteration
for (sample_name in names(species_importance_results)) {
  sample_results <- species_importance_results[[sample_name]]
  
  # Loop through each species result in the sample
  for (species in names(sample_results)) {
    imp_vec <- sample_results[[species]]$top_features
    
    if (!is.null(imp_vec) && length(imp_vec) > 0) {
      # Create a data frame for top features
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

# Combine all into one long-format data frame
importance_df <- bind_rows(importance_list)

# Save to CSV
write.csv(importance_df,
          "E:/Thesis_Final_Data/Analysis/12species_feature_importance_object_based_long.csv",
          row.names = FALSE)


##########################################################################

# Create output folder if needed
output_dir <- "E:/Git Paint Rock 1.0/Output/Analysis/Feature_Boxplots_ObjectBased/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get unique species from importance data
all_species <- unique(importance_df$Species)

# Loop through each species
for (species_to_plot in all_species) {
  
  # Filter importance data for the current species
  df_plot <- importance_df %>%
    filter(Species == species_to_plot)
  
  # Count how often each feature appears across samples (e.g., in top 15)
  feature_counts <- df_plot %>%
    group_by(Feature) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(Count)  # Ascending to set order
  
  # Join back to add counts for labeling
  df_plot <- df_plot %>%
    left_join(feature_counts, by = "Feature")
  
  # Order factor levels of Feature by frequency for consistent plot order
  df_plot$Feature <- factor(df_plot$Feature, levels = feature_counts$Feature)
  
  # Plot feature importance distributions
  p <- ggplot(df_plot, aes(x = Feature, y = Importance)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Feature Importance (Object-Based) for", species_to_plot),
      x = "Feature (n = top occurrences)",
      y = "Mean Decrease Gini"
    ) +
    scale_x_discrete(labels = function(x) {
      counts <- feature_counts$Count[match(x, feature_counts$Feature)]
      paste0(x, " (n=", counts, ")")
    })
  
  # Save the plot to file
  ggsave(
    filename = file.path(output_dir, paste0("Feature_Importance_", species_to_plot, ".png")),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  message("✅ Saved feature importance plot for species: ", species_to_plot)
}
##################################################################################

# 1. Calculate mean importance per feature per species (only if present in >=4 samples)
species_feature_means <- importance_df %>%
  group_by(Species, Feature) %>%
  filter(n() >= 4) %>%  # Retain only features seen in at least 4 samples for a species
  summarise(MeanImportance = mean(Importance), .groups = "drop")

# 2. Initialize vector to store selected features
selected_features <- c()

# 3. Track per-species top features, avoiding duplication
species_list <- unique(species_feature_means$Species)

while (length(selected_features) < 24) {
  for (sp in species_list) {
    sp_feats <- species_feature_means %>%
      filter(Species == sp, !(Feature %in% selected_features)) %>%
      arrange(desc(MeanImportance))
    
    if (nrow(sp_feats) > 0) {
      # Add the next best unselected feature
      next_best_feat <- sp_feats$Feature[1]
      selected_features <- unique(c(selected_features, next_best_feat))
    }
    
    if (length(selected_features) >= 24) break
  }
}

# 4. Construct summary table: rows = selected features, columns = species
final_summary_df <- species_feature_means %>%
  filter(Feature %in% selected_features) %>%
  tidyr::pivot_wider(names_from = Species, values_from = MeanImportance) %>%
  arrange(match(Feature, selected_features))  # Keep feature order consistent

# 5. Save to CSV
write.csv(final_summary_df,
          "E:/Thesis_Final_Data/Analysis/Top24_Feature_Species_MeanImportance_object_based.csv",
          row.names = FALSE)


################################################################################
