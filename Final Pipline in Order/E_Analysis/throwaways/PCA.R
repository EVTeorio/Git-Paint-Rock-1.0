

library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)

# 1. Select metrics and grouping variables
df_metrics <- df %>%
  select(TreeID, SpeciesID, all_of(metrics)) %>%
  filter(if_all(all_of(metrics), ~ !is.na(.)))  # Remove NAs

# Summary: count of unique TreeIDs per SpeciesID
df_summary <- df %>%
  group_by(SpeciesID) %>%
  summarise(Unique_TreeIDs = n_distinct(TreeID))


# 2. Scale metrics (standardize)
scaled_metrics <- scale(df_metrics[, metrics])

# 3. Run PCA
pca_result <- prcomp(scaled_metrics, center = TRUE, scale. = TRUE)

# 4. PCA summary - variance explained
summary(pca_result)

# 5. Extract PCA scores (PC coordinates for each pixel)
pca_scores <- as.data.frame(pca_result$x)

# 6. Add TreeID and SpeciesID back
pca_scores <- cbind(pca_scores, df_metrics %>% select(TreeID, SpeciesID))

# 7. PCA plots
ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(TreeID))) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(color = "TreeID", title = "PCA of Pixels Colored by TreeID")

ggplot(pca_scores, aes(x = PC1, y = PC2, color = SpeciesID)) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(color = "SpeciesID", title = "PCA of Pixels Colored by SpeciesID")

# 8. --- Variance Calculations ---

# A. Within-tree variance by SpeciesID
within_tree_var <- pca_scores %>%
  group_by(SpeciesID, TreeID) %>%
  summarise(
    var_PC1 = var(PC1, na.rm = TRUE),
    var_PC2 = var(PC2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(SpeciesID) %>%
  summarise(
    mean_within_PC1 = mean(var_PC1, na.rm = TRUE),
    mean_within_PC2 = mean(var_PC2, na.rm = TRUE)
  )

# B. Between-tree variance (across all trees)
between_tree_var <- pca_scores %>%
  group_by(TreeID) %>%
  summarise(
    mean_PC1 = mean(PC1, na.rm = TRUE),
    mean_PC2 = mean(PC2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  summarise(
    var_between_PC1 = var(mean_PC1, na.rm = TRUE),
    var_between_PC2 = var(mean_PC2, na.rm = TRUE)
  )

# C. Between-tree variance by SpeciesID
between_tree_var_by_species <- pca_scores %>%
  group_by(SpeciesID, TreeID) %>%
  summarise(mean_PC1 = mean(PC1), mean_PC2 = mean(PC2), .groups = "drop") %>%
  group_by(SpeciesID) %>%
  summarise(
    var_between_PC1 = var(mean_PC1, na.rm = TRUE),
    var_between_PC2 = var(mean_PC2, na.rm = TRUE)
  )

# 9. --- Feature Importance from PCA Loadings ---

# Overall PCA loadings (contributions of metrics to PC1 and PC2)
loading_df <- as.data.frame(pca_result$rotation[, 1:2]) %>%
  rownames_to_column("Metric") %>%
  mutate(
    Abs_PC1 = abs(PC1),
    Abs_PC2 = abs(PC2),
    Combined = sqrt(Abs_PC1^2 + Abs_PC2^2)
  ) %>%
  arrange(desc(Combined))

# 10. Per-species PCA and top metrics
top_metrics_by_species <- df_metrics %>%
  group_by(SpeciesID) %>%
  group_map(~ {
    species_scaled <- scale(.x[, metrics])
    species_pca <- prcomp(species_scaled, center = TRUE, scale. = TRUE)
    loadings <- as.data.frame(species_pca$rotation[, 1:2]) %>%
      rownames_to_column("Metric") %>%
      mutate(
        Abs_PC1 = abs(PC1),
        Abs_PC2 = abs(PC2),
        Combined = sqrt(Abs_PC1^2 + Abs_PC2^2)
      ) %>%
      arrange(desc(Combined))
    loadings$SpeciesID <- unique(.x$SpeciesID)
    return(loadings)
  }) %>%
  bind_rows()

# --- Print Outputs ---
print("Within-tree variance by species:")
print(within_tree_var)

print("Between-tree variance overall:")
print(between_tree_var)

print("Between-tree variance by species:")
print(between_tree_var_by_species)

print("Top contributing metrics overall:")
print(loading_df)

print("Top contributing metrics by species:")
print(top_metrics_by_species)

