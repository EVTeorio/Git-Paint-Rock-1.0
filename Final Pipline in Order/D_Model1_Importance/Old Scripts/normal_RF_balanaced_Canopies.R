

# Load data
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean_sunlit_no_nas.csv")
beep()
str(spec_chem_canopy)

# Get canopy and species info
canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

# Get species counts for training eligibility
species_counts <- canopies %>%
  count(SpeciesID)

# Species with at least 6 canopies (4 train + 2 test)
species_for_balanced_sampling <- species_counts %>%
  filter(n >= 6) %>%
  pull(SpeciesID)

# Define metrics
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
  "PAD_30_35_off", "PAD_35_40_off", "PAD_5_10_off",
  "PAD_0_5_on", "PAD_10_15_on", "PAD_15_20_on", "PAD_20_25_on", "PAD_25_30_on",
  "PAD_30_35_on", "PAD_35_40_on", "PAD_5_10_on",
  "Seasonal_Occupancy_20_35m"
)

# Precompute canopy-averaged data (keep all species)
canopy_means <- spec_chem_canopy %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE), .groups = "drop")

all_results <- list()
importance_results <- list()

# Loop over 10 iterations
for (i in 1:10) {
  cat("\n--- Balanced Sample Iteration", i, "---\n")
  set.seed(200 + i)
  
  # Sample 4 training canopies per eligible species
  train_canopies <- canopy_means %>%
    filter(SpeciesID %in% species_for_balanced_sampling) %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 4) %>%
    ungroup()
  
  # Test canopies = all others not in training (from ALL species)
  test_canopies <- canopy_means %>%
    filter(!TreeID %in% train_canopies$TreeID) %>%
    ungroup()
  
  train_canopies$SpeciesID <- as.factor(train_canopies$SpeciesID)
  test_canopies$SpeciesID <- as.factor(test_canopies$SpeciesID)
  
  # Prepare training and test data
  train_data <- train_canopies %>%
    select(SpeciesID, all_of(metrics))
  
  test_data <- test_canopies %>%
    select(SpeciesID, all_of(metrics))
  
  # Train Random Forest model
  rf_model <- randomForest(SpeciesID ~ ., data = train_data, ntree = 3000, importance = TRUE)
  
  # Predict on test data
  preds <- predict(rf_model, newdata = test_data)
  
  # Evaluate
  cm <- confusionMatrix(preds, test_data$SpeciesID)
  
  precision <- cm$byClass[, "Precision"]
  recall <- cm$byClass[, "Recall"]
  f1_by_class <- 2 * (precision * recall) / (precision + recall)
  f1_macro <- mean(f1_by_class, na.rm = TRUE)
  
  # Store results
  all_results[[paste0("Sample_", i)]] <- list(
    model = rf_model,
    confusion = cm,
    accuracy = cm$overall["Accuracy"],
    f1_by_class = f1_by_class,
    f1_macro = f1_macro
  )
  
  importance_results[[paste0("Sample_", i)]] <- importance(rf_model)
}

beep(3)
