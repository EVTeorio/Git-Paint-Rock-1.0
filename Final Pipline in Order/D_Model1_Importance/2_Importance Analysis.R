

importance_results <- readRDS("E:/Results/Balanced_Full_RF_importance_all_metrics.rds")

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)

# Step 1: Extract MeanDecreaseAccuracy per feature from each sample
mda_df <- map_dfr(
  .x = names(importance_results),
  .f = function(sample_name) {
    df <- as.data.frame(importance_results[[sample_name]])
    tibble(
      Feature = rownames(df),
      MeanDecreaseAccuracy = df$MeanDecreaseAccuracy,
      Sample = sample_name
    )
  }
)

# Step 2: Identify top 30 features by average importance
top_features <- mda_df %>%
  group_by(Feature) %>%
  summarise(AvgMDA = mean(MeanDecreaseAccuracy, na.rm = TRUE), .groups = "drop") %>%
  top_n(30, AvgMDA) %>%
  pull(Feature)

# Step 3: Filter the main data to include only top features
mda_top30 <- mda_df %>%
  filter(Feature %in% top_features)

# Step 4: Plot with flipped axes
ggplot(mda_top30, aes(y = reorder(Feature, MeanDecreaseAccuracy, FUN = mean), x = MeanDecreaseAccuracy)) +
  geom_boxplot(outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, fill = "red", color = "black") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Top 30 Features by MeanDecreaseAccuracy",
    y = "Feature",
    x = "MeanDecreaseAccuracy",
    caption = "Red dots indicate mean across samples"
  )

# Step 1: Calculate average MeanDecreaseAccuracy for each feature
top30_summary <- mda_df %>%
  group_by(Feature) %>%
  summarise(AvgMDA = mean(MeanDecreaseAccuracy, na.rm = TRUE), .groups = "drop") %>%
  #top_n(20, AvgMDA) %>%
  arrange(desc(AvgMDA))  # Arrange for prettier bar plot

# Step 2: Create bar plot
ggplot(top30_summary, aes(x = reorder(Feature, AvgMDA), y = AvgMDA)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Top 30 Features by Average MeanDecreaseAccuracy",
    x = "Feature",
    y = "Average MeanDecreaseAccuracy"
  )
#############################################################################

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

# Preview the data
print(species_importance_df, n = 20)
##########################################################################
######## Selecting top importance metrics##########################
# 1. Calculate mean importance per feature per species
species_feature_means <- species_importance_df %>%
  group_by(SpeciesID, Feature) %>%
  summarise(MeanImportance = mean(ImportanceValue, na.rm = TRUE), .groups = "drop")

# Convert species_feature_means to wide format: species as rows, features as columns
feature_matrix <- species_feature_means %>%
  pivot_wider(names_from = SpeciesID, values_from = MeanImportance)


# Join top30_summary with feature_matrix by Feature
combined_df <- left_join(feature_matrix, top30_summary, by = "Feature")

# View the result
print(n=30,combined_df)

# Save to CSV (optional)
write.csv(combined_df, "E:/Results/species_feature_importance_by_sample.csv", row.names = FALSE)

# 2. Initialize empty vector for selected features
selected_features <- c()

# 3. Get list of unique species
species_list <- unique(species_feature_means$SpeciesID)

# 4. Iteratively select top feature per species (skipping already selected ones)
while (length(selected_features) < 25) {
  for (sp in species_list) {
    sp_feats <- species_feature_means %>%
      filter(SpeciesID == sp, !(Feature %in% selected_features)) %>%
      arrange(desc(MeanImportance))
    
    if (nrow(sp_feats) > 0) {
      next_best_feat <- sp_feats$Feature[1]
      selected_features <- unique(c(selected_features, next_best_feat))
    }
    
    if (length(selected_features) >= 25) break
  }
}

# 5. Construct summary table: rows = selected features, columns = species
final_summary_df <- species_feature_means %>%
  filter(Feature %in% selected_features) %>%
  pivot_wider(names_from = SpeciesID, values_from = MeanImportance) %>%
  arrange(match(Feature, selected_features))  # Maintain feature selection order

print(selected_features)

# 6. Save the summary to CSV
write.csv(final_summary_df,
          "E:/Results/Top25_Feature_Species_MeanImportance_object_based.csv",
          row.names = FALSE)
#################################################################################

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

#############Standard Deviation
species_feature_means <- species_importance_df %>%
  group_by(SpeciesID, Feature) %>%
  summarise(
    MeanImportance = mean(ImportanceValue, na.rm = TRUE),
    SDImportance = sd(ImportanceValue, na.rm = TRUE),
    CVImportance = ifelse(
      MeanImportance != 0,
      SDImportance / MeanImportance,
      NA_real_
    ),
    .groups = "drop"
  )

