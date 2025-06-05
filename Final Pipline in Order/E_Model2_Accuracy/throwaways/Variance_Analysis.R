


# ---- Load Packages ----
library(dplyr)
library(tidyr)
library(randomForest)
library(ggplot2)

# ---- Prepare Data ----
# Remove rows with NA in any selected metric
df_metrics <- df %>%
  select(TreeID, SpeciesID, all_of(metrics)) %>%
  filter(if_all(all_of(metrics), ~ !is.na(.)))

# ---- 1. Identify Metrics That Distinguish Tree Species ----

# Fit random forest model to classify species
set.seed(123)
rf_model <- randomForest(SpeciesID ~ ., data = df_sampled, ntree = 300, importance = TRUE)
beep(3)
# Get variable importance
importance_df <- as.data.frame(importance(rf_model, type = 1)) %>%
  rownames_to_column("Metric") %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  rename(Importance = MeanDecreaseAccuracy)

# Visualize top differentiating metrics
top_n <- 15  # adjust as needed
ggplot(head(importance_df, top_n), aes(x = reorder(Metric, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Metrics for Species Differentiation (Random Forest)",
       x = "Metric", y = "Importance")

# ---- 2. Identify Metrics That Contribute to Tree-Level Heterogeneity ----

# Per-species within-tree variance for each metric
within_tree_var_by_species <- df_metrics %>%
  group_by(SpeciesID, TreeID) %>%
  summarise(across(all_of(metrics), var, na.rm = TRUE), .groups = "drop") %>%
  group_by(SpeciesID) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -SpeciesID, names_to = "Metric", values_to = "MeanTreeVar")

# Visualize top metrics per species for within-tree heterogeneity
ggplot(within_tree_var_by_species, aes(x = reorder(Metric, MeanTreeVar), y = MeanTreeVar, fill = SpeciesID)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Metrics Contributing to Within-Tree Variability by Species",
       x = "Metric", y = "Mean Within-Tree Variance")

# Optionally: Top metrics per species
top_heterogeneity_metrics <- within_tree_var_by_species %>%
  group_by(SpeciesID) %>%
  slice_max(order_by = MeanTreeVar, n = 5)

# Print Summary Tables
cat("\n--- Top Metrics Differentiating Species (Random Forest Importance) ---\n")
print(head(importance_df, 60))

cat("\n--- Top Metrics Contributing to Tree-Level Heterogeneity per Species ---\n")
print(n = 60, top_heterogeneity_metrics)

