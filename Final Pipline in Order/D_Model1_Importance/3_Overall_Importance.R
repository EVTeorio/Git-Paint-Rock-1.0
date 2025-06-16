

importance_results <- readRDS("E:/Results/Balanced_Full_RF_importance_all_metrics.rds")


# Extract only MeanDecreaseAccuracy for each feature across all samples
mean_accuracy_df <- map_dfr(
  .x = names(importance_results),
  .f = function(sample_name) {
    imp_matrix <- importance_results[[sample_name]]
    
    # Calculate mean across all species (i.e., columns)
    mean_imp <- rowMeans(imp_matrix, na.rm = TRUE)
    
    tibble(
      Feature = names(mean_imp),
      MeanDecreaseAccuracy = mean_imp,
      Sample = sample_name
    )
  }
)

print(mean_accuracy_df)
# Compute mean importance across all samples
top_15_features <- mean_accuracy_df %>%
  group_by(Feature) %>%
  summarise(MeanImportance = mean(MeanDecreaseAccuracy, na.rm = TRUE)) %>%
  arrange(desc(MeanImportance)) %>%
  slice_head(n = 15) %>%
  pull(Feature)

top_15_df <- mean_accuracy_df %>%
  filter(Feature %in% top_15_features)

ggplot(top_15_df, aes(x = reorder(Feature, -MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Distribution of Mean Decrease Accuracy (Top 15 Features)",
    x = "Feature",
    y = "Mean Decrease Accuracy"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################
top_15_summary <- top_15_df %>%
  group_by(Feature) %>%
  summarise(MeanAccuracy = mean(MeanDecreaseAccuracy, na.rm = TRUE))

ggplot(top_15_summary, aes(x = reorder(Feature, MeanAccuracy), y = MeanAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Mean Decrease Accuracy (Top 15 Vegetation Indices)",
    x = "Vegetation Index",
    y = "Average Importance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  coord_flip()



overall_importance <- mean_accuracy_df %>%
  group_by(Feature) %>%
  summarise(
    MeanImportance = mean(MeanDecreaseAccuracy, na.rm = TRUE),
    SDImportance = sd(MeanDecreaseAccuracy, na.rm = TRUE)
  ) %>%
  arrange(desc(MeanImportance))


print(overall_importance)
