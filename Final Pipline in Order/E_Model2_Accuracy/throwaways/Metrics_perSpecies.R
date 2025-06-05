

# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(pdp)
library(vegan)
library(Boruta)
library(beepr)

# Read the original dataset
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/Balanced_Dataframes/Balanced_Sample_10.csv")

# Step 1: Clean data
# Remove non-numeric and irrelevant columns
data_clean <- spec_chem_canopy %>%
  select(-X, -TileNumber, -TreeID)

# Remove any rows with NA values
data_clean <- na.omit(data_clean)

# Convert SpeciesID to factor
data_clean$SpeciesID <- as.factor(data_clean$SpeciesID)

# Separate the species ID from the metrics
species <- data_clean$SpeciesID
metrics <- data_clean %>%
  select(-SpeciesID)

# Make sure all columns are numeric
metrics <- metrics %>% mutate(across(everything(), as.numeric))

# Step 2: Run adonis2 to assess overall correlation
adonis_full <- adonis2(metrics ~ species, method = "euclidean", permutations = 10)
beep(3)
# Print overall PERMANOVA result
print("Full PERMANOVA result:")
print(adonis_full)

# Step 3: Run adonis2 marginally for each metric
# This will give us the importance of each individual variable
metric_importance <- lapply(names(metrics), function(metric) {
  formula <- as.formula(paste(metric, "~ species"))
  result <- adonis2(as.formula(paste0("metrics[['", metric, "']] ~ species")), method = "euclidean", permutations = 999)
  data.frame(
    Metric = metric,
    R2 = result$R2[1],
    p = result$`Pr(>F)`[1]
  )
})

# Combine results into a single data frame
importance_df <- do.call(rbind, metric_importance)

# Step 4: Sort metrics by R2 value (variance explained)
importance_df_sorted <- importance_df %>%
  arrange(desc(R2))

# Show the top 10 most important metrics
print("Top 10 most important metrics based on R²:")
print(head(importance_df_sorted, 10))

# Optional: Save results to CSV
write.csv(importance_df_sorted, "metric_importance_speciesID.csv", row.names = FALSE)

###########################################################3

set.seed(42)
boruta_out <- Boruta(SpeciesID ~ ., data = data_clean, doTrace = 1)

# Get confirmed important features
confirmed <- getSelectedAttributes(boruta_out, withTentative = FALSE)
confirmed
