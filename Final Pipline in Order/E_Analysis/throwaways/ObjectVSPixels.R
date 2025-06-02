


# ---- Required Libraries ----
library(vegan)
library(dplyr)

# ---- Step 1: Aggregate Data to Canopy Level ----
canopy_df <- df %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean), .groups = "drop")

# Scale canopy-level metrics
canopy_scaled <- scale(canopy_df[, metrics])

# Also aggregate pixel-level data to same TreeID-level for fair comparison
pixel_df <- df %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean), .groups = "drop")

pixel_scaled <- scale(pixel_df[, metrics])

# ---- Step 2: Procrustes Test ----
# Compares ordination structures (requires matching dimensions)
pro <- protest(pixel_scaled, canopy_scaled, permutations = 999)
cat("\n--- Procrustes Test ---\n")
print(pro)

# ---- Step 3: Mantel Test ----
# Compares distance matrices (Pearson correlation of distance matrices)
dist_pixel <- dist(pixel_scaled)
dist_canopy <- dist(canopy_scaled)

mantel_result <- mantel(dist_pixel, dist_canopy, method = "pearson", permutations = 999)
cat("\n--- Mantel Test ---\n")
print(mantel_result)
