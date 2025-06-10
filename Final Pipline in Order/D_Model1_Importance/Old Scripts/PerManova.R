

library(tidyverse)
library(vegan)

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

# ---- Parameters ----
set.seed(42)  # For reproducibility
pixels_per_tree <- 100  # Adjust based on data volume

# ---- Step 1: Prepare and Sample Pixel-Level Data ----

# Stratified sampling: sample up to N pixels per tree
df_sampled <- df %>%
  group_by(TreeID) %>%
  sample_n(size = min(pixels_per_tree, n()), replace = FALSE) %>%
  ungroup()

# ---- Step 2: Standardize Pixel Metrics ----

scaled_sampled_metrics <- scale(df_sampled[, metrics])

# ---- Step 3: PERMANOVA on Sampled Pixels ----

# A. Species only
adonis_species_sampled <- adonis2(
  scaled_sampled_metrics ~ SpeciesID,
  data = df_sampled,
  method = "euclidean"
)
beep(3)

# B. Species / TreeID nested
adonis_nested_sampled <- adonis2(
  scaled_sampled_metrics ~ SpeciesID / TreeID,
  data = df_sampled,
  method = "euclidean"
)

# ---- Step 4: Prepare Canopy-Level Data ----

# Aggregate pixel metrics per TreeID (mean)
canopy_df <- df %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean), .groups = "drop")

# Scale canopy-level metrics
canopy_scaled <- scale(canopy_df[, metrics])

# ---- Step 5: PERMANOVA on Canopy-Level Data ----

adonis_canopy_species <- adonis2(
  canopy_scaled ~ SpeciesID,
  data = canopy_df,
  method = "euclidean"
)

# ---- Step 6: Output Results ----

cat("\n--- PERMANOVA on Sampled Pixels ---\n")
cat("A. Variation explained by Species:\n")
print(adonis_species_sampled)

cat("\nB. Variation explained by TreeID nested within Species:\n")
print(adonis_nested_sampled)

cat("\n--- PERMANOVA on Summarized Canopy-Level Data ---\n")
cat("C. Variation explained by Species (Canopy Aggregated):\n")
print(adonis_canopy_species)

