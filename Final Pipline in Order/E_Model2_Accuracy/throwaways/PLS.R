install.packages("mix0mics")
# ---- Load Required Packages ----
library(mixOmics)     # For PLS-DA
library(dplyr)
library(tidyr)
library(ggplot2)


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

# ---- Step 1: Prepare Data ----
df_metrics <- df %>%
  select(TreeID, SpeciesID, all_of(metrics)) %>%
  filter(if_all(all_of(metrics), ~ !is.na(.)))

X <- df_metrics[, metrics]
Y <- as.factor(df_metrics$SpeciesID)

# ---- Step 2: Run PLS-DA ----
set.seed(42)
plsda_model <- plsda(X, Y, ncomp = 3)

# ---- Step 3: Extract Variable Importance ----
vip_scores <- vip(plsda_model)
vip_df <- data.frame(
  Metric = rownames(vip_scores),
  VIP = vip_scores[, 1]  # use first component, or sum across components
) %>%
  arrange(desc(VIP))

# ---- Step 4: Plot Top Species-Discriminating Metrics ----
ggplot(head(vip_df, 15), aes(x = reorder(Metric, VIP), y = VIP)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top Metrics for Species Discrimination (PLS-DA)",
       x = "Metric", y = "VIP Score")

cat("\n--- Top 10 Metrics by PLS-DA VIP Scores ---\n")
print(head(vip_df, 10))

# ---- Step 5: Metrics Contributing to Within-Tree Variance ----
within_tree_var_by_species <- df_metrics %>%
  group_by(SpeciesID, TreeID) %>%
  summarise(across(all_of(metrics), var, na.rm = TRUE), .groups = "drop") %>%
  group_by(SpeciesID) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -SpeciesID, names_to = "Metric", values_to = "MeanTreeVar")

# ---- Step 6: Plot Within-Tree Variability ----
ggplot(within_tree_var_by_species, aes(x = reorder(Metric, MeanTreeVar), y = MeanTreeVar, fill = SpeciesID)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Metrics Driving Within-Tree Variability by Species",
       x = "Metric", y = "Mean Within-Tree Variance")

# ---- Optional: Print Top Heterogeneity Metrics ----
top_heterogeneity_metrics <- within_tree_var_by_species %>%
  group_by(SpeciesID) %>%
  slice_max(order_by = MeanTreeVar, n = 5)

cat("\n--- Top Metrics Driving Tree-Level Heterogeneity per Species ---\n")
print(top_heterogeneity_metrics)
