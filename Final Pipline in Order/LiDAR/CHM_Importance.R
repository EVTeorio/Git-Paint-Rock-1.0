

# Load necessary libraries
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)
library(purrr)
library(adonis2)

# File paths
canopies_path <- "E:/Git Paint Rock 1.0/Updated Canopy Polygons/Updated/"
liDAR_CHM_path <- "E:/Updated LiDAR/PRFPD_CHM_leafOn.tiff"

# Load CHM raster
chm_raster <- rast(liDAR_CHM_path)
chm_crs <- crs(chm_raster)

# List shapefiles
shapefiles <- list.files(canopies_path, pattern = "\\.shp$", full.names = TRUE)

# Initialize list for pixel-level dataframes
pixels_list <- list()

for (shp_path in shapefiles) {
  cat("Processing:", basename(shp_path), "\n")
  
  # Read shapefile
  canopy_sf <- st_read(shp_path, quiet = TRUE)
  
  # Reproject to CHM raster CRS
  canopy_sf <- st_transform(canopy_sf, crs = chm_crs)
  
  # Extract pixel values *inside* polygons with polygon IDs
  # This returns a list of data.frames, one per polygon
  extracted_pixels <- exact_extract(
    chm_raster, 
    canopy_sf,
    include_cols = "Canopies",
    progress = FALSE
  )
  
  # Combine all polygon pixels into one df with polygon ID
  polygon_pixels_df <- map_df(extracted_pixels, ~ {
    # Each .x is a data.frame with columns: value (pixel height), coverage_fraction, Canopies
    # We keep pixels with non-NA values
    df <- filter(.x, !is.na(value))
    # Keep only value and Canopies columns
    df %>% select(Canopies, value)
  })
  
  pixels_list[[basename(shp_path)]] <- polygon_pixels_df
}

# Combine pixels from all shapefiles
all_pixels_df <- bind_rows(pixels_list, .id = "Shapefile_Source")

# Split Canopies into TreeID and SpeciesID
all_pixels_df <- all_pixels_df %>%
  separate(Canopies, into = c("TreeID", "SpeciesID"), sep = "_", remove = TRUE) %>%
  rename(Height = value)

write.csv(all_pixels_df, "Canopy_Pixel_Height_Data.csv", row.names = FALSE)

#####################################################################

# Load your pixel-level data (assumed already extracted)
df <- all_pixels_df %>%
  filter(!is.na(Height)) %>%
  select(TreeID, SpeciesID, Height)

# Keep species with >10 canopies
species_counts <- df %>%
  distinct(TreeID, SpeciesID) %>%
  count(SpeciesID) %>%
  filter(n > 10)

df <- df %>%
  filter(SpeciesID %in% species_counts$SpeciesID)

# Loop for 10 iterations
species_importance_results <- list()

for (i in 1:10) {
  cat("\n--- Pixel-Based Sample", i, "---\n")
  set.seed(100 + i)
  
  # Sample ~70% of pixels per species for training
  train_df <- df %>%
    group_by(SpeciesID) %>%
    slice_sample(prop = 0.7) %>%
    ungroup()
  
  # Sample ~30% of the rest as test (up to 2000 per species)
  test_df <- anti_join(df, train_df, by = c("TreeID", "SpeciesID", "Height")) %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 100, replace = TRUE) %>%
    ungroup()
  
  # Balance training data: up to 200 pixels per TreeID
  balanced_train_df <- train_df %>%
    group_by(TreeID) %>%
    slice_sample(n = 50, replace = TRUE) %>%
    ungroup()
  
  balanced_train_df$SpeciesID <- as.factor(balanced_train_df$SpeciesID)
  test_df$SpeciesID <- as.factor(test_df$SpeciesID)
  
  # One-vs-rest classification
  species_list <- unique(balanced_train_df$SpeciesID)
  binary_results <- list()
  
  for (species in species_list) {
    cat("  --> One-vs-rest for species:", species, "\n")
    
    train_binary <- balanced_train_df %>%
      mutate(binary_label = ifelse(SpeciesID == species, "target", "other")) %>%
      select(binary_label, Height)
    
    test_binary <- test_df %>%
      mutate(binary_label = ifelse(SpeciesID == species, "target", "other")) %>%
      select(binary_label, Height)
    
    train_binary$binary_label <- factor(train_binary$binary_label, levels = c("other", "target"))
    test_binary$binary_label <- factor(test_binary$binary_label, levels = c("other", "target"))
    
    # Skip if not enough diversity
    if (length(unique(train_binary$binary_label)) < 2 || length(unique(test_binary$binary_label)) < 2) {
      warning(paste("Not enough data for species", species))
      next
    }
    
    rf_bin <- randomForest(binary_label ~ Height, data = train_binary, ntree = 1000, importance = TRUE)
    pred_bin <- predict(rf_bin, test_binary)
    
    cm_bin <- confusionMatrix(pred_bin, test_binary$binary_label, positive = "target")
    
    imp_bin <- importance(rf_bin)[, "MeanDecreaseGini", drop = FALSE]
    imp_bin <- sort(imp_bin[, 1], decreasing = TRUE)
    
    binary_results[[as.character(species)]] <- list(
      accuracy = cm_bin$overall["Accuracy"],
      precision = cm_bin$byClass["Precision"],
      recall = cm_bin$byClass["Recall"],
      f1 = cm_bin$byClass["F1"],
      top_features = imp_bin
    )
  }
  
  species_importance_results[[paste0("Pixel_Sample_", i)]] <- binary_results
}
beep()


# Initialize list to collect rows
summary_list <- list()

for (sample_name in names(species_importance_results)) {
  sample_results <- species_importance_results[[sample_name]]
  
  for (species in names(sample_results)) {
    res <- sample_results[[species]]
    
    # Directly pull the numeric top feature value
    importance_value <- if (!is.null(res$top_features)) {
      as.numeric(res$top_features)
    } else {
      NA
    }
    
    # Create a one-row data frame with metrics
    row <- data.frame(
      Sample = sample_name,
      Species = species,
      Accuracy = as.numeric(res$accuracy),
      Precision = as.numeric(res$precision),
      Recall = as.numeric(res$recall),
      F1 = as.numeric(res$f1),
      Height_Importance = importance_value,
      stringsAsFactors = FALSE
    )
    
    summary_list[[paste(sample_name, species, sep = "_")]] <- row
  }
}

# Combine all into one data frame
species_summary_df <- bind_rows(summary_list)
###############################################################
# Boxplot for F1 Scores per Species
ggplot(species_summary_df, aes(x = Species, y = F1, fill = Species)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "F1 Scores per Species",
       y = "F1 Score",
       x = "Species") +
  theme(legend.position = "none")

# Boxplot for Height Importance per Species
ggplot(species_summary_df, aes(x = Species, y = Height_Importance, fill = Species)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Height Importance per Species",
       y = "Height Importance (MeanDecreaseGini)",
       x = "Species") +
  theme(legend.position = "none")
##########################################################################3
#Canopy PerMoanova
# Select features and remove NAs
canopy_metrics <- final_results %>%
  select(max) %>%
  drop_na()

# Corresponding SpeciesID
canopy_species <- final_results %>%
  filter(!is.na(max)) %>%
  pull(SpeciesID)

# Compute distance matrix
canopy_dist <- dist(canopy_metrics)

# Run PERMANOVA
adonis_canopy <- adonis2(canopy_dist ~ canopy_species, permutations = 999)
print(adonis_canopy)

#####Pixel Permanova

#Using height as a single feature
pixel_dist <- dist(all_pixels_df %>% select(Height))

# Run PERMANOVA
adonis_pixel <- adonis2(pixel_dist ~ SpeciesID, data = all_pixels_df, permutations = 999)
print(adonis_pixel)
beep()
