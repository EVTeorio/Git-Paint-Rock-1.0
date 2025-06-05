

iter_results <- readRDS("iter_results_500.rds")

# Clean out NA/NaN F1 scores
f1_clean <- f1_all %>%
  filter(!is.na(F1), !is.nan(F1))

# Count number of valid F1 scores per species, rename count to avoid conflict
species_counts <- f1_clean %>%
  count(Species, name = "sample_count")

# Join counts to f1_clean and create custom x-axis labels
f1_clean <- f1_clean %>%
  left_join(species_counts, by = "Species") %>%
  mutate(Species_label = paste0(Species, "\n(n=", sample_count, ")"))

# Plot boxplot with custom labels
ggplot(f1_clean, aes(x = Species_label, y = F1)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.color = "gray40") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Per-Species F1 Score Distribution Across Iterations",
    x = "Species",
    y = "F1 Score"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
