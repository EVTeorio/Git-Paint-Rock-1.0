

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tibble)

# Define metrics to analyze (you can modify this)
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
  "PAD_30_35_off", "PAD_35_40_off", "PAD_40_45_off", "PAD_45_50_off", "PAD_5_10_off",
  "PAD_0_5_on", "PAD_10_15_on", "PAD_15_20_on", "PAD_20_25_on", "PAD_25_30_on",
  "PAD_30_35_on", "PAD_35_40_on", "PAD_40_45_on", "PAD_45_50_on", "PAD_5_10_on",
  "Seasonal_Occupancy_20_35m"
)


# Subset data
df <- train_df[, c("TreeID", "SpeciesID", metrics)]

# 1. Select metrics and remove rows with missing values
df_metrics <- df %>%
  select(TreeID, SpeciesID, all_of(metrics)) %>%
  filter(if_all(all_of(metrics), ~ !is.na(.)))

# 2. Scale metrics
scaled_metrics <- scale(df_metrics[, metrics])

# 3. PCA on all data (pixel-level)
pca_result <- prcomp(scaled_metrics, center = TRUE, scale. = TRUE)
pca_scores <- as.data.frame(pca_result$x)
pca_scores <- bind_cols(df_metrics %>% select(TreeID, SpeciesID), pca_scores)

# 4. Total within-tree variance per species
within_tree_var_total <- pca_scores %>%
  group_by(SpeciesID, TreeID) %>%
  summarise(total_var = sum(apply(across(starts_with("PC")), 2, var)), .groups = "drop") %>%
  group_by(SpeciesID) %>%
  summarise(mean_within_tree_total_var = mean(total_var, na.rm = TRUE))

# 5. Total between-tree variance per species
between_tree_var_total <- pca_scores %>%
  group_by(SpeciesID, TreeID) %>%
  summarise(across(starts_with("PC"), mean), .groups = "drop") %>%
  group_by(SpeciesID) %>%
  summarise(total_var = sum(across(starts_with("PC"), ~ var(.x, na.rm = TRUE))))

# 6. PCA at the canopy level (1 row per TreeID = 1 canopy object)
canopy_df <- df %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE), .groups = "drop")

canopy_scaled <- scale(canopy_df[, metrics])
canopy_pca <- prcomp(canopy_scaled, center = TRUE, scale. = TRUE)
canopy_scores <- as.data.frame(canopy_pca$x)
canopy_scores <- bind_cols(canopy_df %>% select(TreeID, SpeciesID), canopy_scores)

# 7. Variance in canopy-level PCA
canopy_var_total <- canopy_scores %>%
  group_by(SpeciesID) %>%
  summarise(total_var = sum(across(starts_with("PC"), ~ var(.x, na.rm = TRUE))))

# # 8. PCA loadings (metric importance)
# loading_df <- as.data.frame(pca_result$rotation) %>%
#   rownames_to_column("Metric") %>%
#   mutate(Combined = apply(across(starts_with("PC")), 1, function(x) sqrt(sum(x^2)))) %>%
#   arrange(desc(Combined))
# 
# # 9. Per-species PCA + top metrics
# top_metrics_by_species <- df_metrics %>%
#   group_by(SpeciesID) %>%
#   group_map(~ {
#     species_scaled <- scale(.x[, metrics])
#     species_pca <- prcomp(species_scaled, center = TRUE, scale. = TRUE)
#     loadings <- as.data.frame(species_pca$rotation) %>%
#       rownames_to_column("Metric") %>%
#       mutate(Combined = apply(across(starts_with("PC")), 1, function(x) sqrt(sum(x^2)))) %>%
#       arrange(desc(Combined))
#     loadings$SpeciesID <- unique(.x$SpeciesID)
#     loadings
#   }) %>%
#   bind_rows()

# --- 11. Distance of species to global mean ---

# A. Pixel-level distances to overall mean
global_mean_pixel <- colMeans(pca_scores %>% select(starts_with("PC")), na.rm = TRUE)

pixel_distances <- pca_scores %>%
  rowwise() %>%
  mutate(distance = sqrt(sum((c_across(starts_with("PC")) - global_mean_pixel)^2))) %>%
  ungroup()

species_pixel_distance <- pixel_distances %>%
  group_by(SpeciesID) %>%
  summarise(mean_distance_to_global = mean(distance, na.rm = TRUE))

# B. Canopy-level distances to overall mean
global_mean_canopy <- colMeans(canopy_scores %>% select(starts_with("PC")), na.rm = TRUE)

canopy_distances <- canopy_scores %>%
  rowwise() %>%
  mutate(distance = sqrt(sum((c_across(starts_with("PC")) - global_mean_canopy)^2))) %>%
  ungroup()

species_canopy_distance <- canopy_distances %>%
  group_by(SpeciesID) %>%
  summarise(mean_distance_to_global = mean(distance, na.rm = TRUE))

# --- 12. Plots ---

# G. Distance of pixel clouds to global mean
ggplot(species_pixel_distance, aes(x = reorder(SpeciesID, mean_distance_to_global), y = mean_distance_to_global)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Pixel-level Distance to Global Mean (All Species)",
       x = "SpeciesID", y = "Mean Distance")

# H. Distance of canopy means to global mean
ggplot(species_canopy_distance, aes(x = reorder(SpeciesID, mean_distance_to_global), y = mean_distance_to_global)) +
  geom_col(fill = "darkorange3") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Canopy Mean Distance to Global Mean (All Species)",
       x = "SpeciesID", y = "Mean Distance")

# A. PCA plot (pixels)
ggplot(pca_scores, aes(x = PC1, y = PC2, color = SpeciesID)) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "PCA of Pixels Colored by Species", color = "SpeciesID")

# B. PCA plot (canopy-level)
ggplot(canopy_scores, aes(x = PC1, y = PC2, color = SpeciesID)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(title = "PCA of Canopy Means (Each Tree = 1 Point)", color = "SpeciesID")

# C. Barplot: within-tree total variance by species
ggplot(within_tree_var_total, aes(x = SpeciesID, y = mean_within_tree_total_var)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Mean Within-Tree Total Variance by Species",
       y = "Total Variance", x = "SpeciesID") +
  coord_flip()

# D. Barplot: between-tree total variance by species (pixel PCA)
ggplot(between_tree_var_total, aes(x = SpeciesID, y = total_var)) +
  geom_col(fill = "darkred") +
  theme_minimal() +
  labs(title = "Between-Tree Total Variance by Species (Pixel PCA)",
       y = "Total Variance", x = "SpeciesID") +
  coord_flip()

# E. Barplot: between-tree variance from canopy PCA
ggplot(canopy_var_total, aes(x = SpeciesID, y = total_var)) +
  geom_col(fill = "forestgreen") +
  theme_minimal() +
  labs(title = "Variance at Canopy Level PCA (Mean Metrics per Tree)",
       y = "Total Variance", x = "SpeciesID") +
  coord_flip()

# --- PRINT OUTPUTS ---
print("Mean within-tree total variance by species (pixel level):")
print(within_tree_var_total)

print("Between-tree total variance by species (pixel level):")
print(between_tree_var_total)

print("Variance at canopy (object) level PCA:")
print(canopy_var_total)

print("Pixel-level mean distance to global centroid by species:")
print(species_pixel_distance)

print("Canopy-level mean distance to global centroid by species:")
print(species_canopy_distance)

# print("Top contributing metrics overall:")
# print(loading_df)
# 
# print("Top contributing metrics by species:")
# print(top_metrics_by_species)
# 
