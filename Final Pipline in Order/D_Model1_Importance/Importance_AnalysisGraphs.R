
importance_results <- readRDS("E:/Results/Balanced_Full_RF_importance_all_metrics.rds")


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
str(importance_results)
##########################################################################



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


# 1. Calculate mean importance per feature per species
species_feature_means <- species_importance_df %>%
  group_by(SpeciesID, Feature) %>%
  summarise(MeanImportance = mean(ImportanceValue, na.rm = TRUE), .groups = "drop")

# Set output directory
output_dir <- "E:/Results/Feature_Importance_By_Feature/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Loop through each unique feature
for (this_feature in unique(species_feature_means$Feature)) {
  
  # Subset data for the current feature
  df_plot <- species_feature_means %>%
    filter(Feature == this_feature)
  
  # Plot
  p <- ggplot(df_plot, aes(x = reorder(SpeciesID, MeanImportance), y = MeanImportance)) +
    geom_col(fill = "darkorange", alpha = 0.8) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Mean Importance of", this_feature, "by Species"),
      x = "Species",
      y = "Mean Decrease Gini"
    ) +
    theme(axis.text.y = element_text(size = 8))
  
  # Save to disk
  ggsave(
    filename = file.path(output_dir, paste0("FeatureImportance_", this_feature, ".png")),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}
beep()

# Convert species_feature_means to wide format: species as rows, features as columns
feature_matrix <- species_feature_means %>%
  pivot_wider(names_from = Feature, values_from = MeanImportance)


##################################################################################
# 1. Calculate mean importance per feature per species
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
