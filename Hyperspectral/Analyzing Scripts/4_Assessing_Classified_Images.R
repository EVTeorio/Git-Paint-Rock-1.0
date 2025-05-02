


# --- Load required libraries ---
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(caret)
library(fasterize)


# --- File paths (EDIT as needed) ---
classified_raster_path <- "E:/Git Paint Rock 1.0/Output/Classified_Images/7995_classified_sunlit.tif"
shapefile_path <- "E:/Git Paint Rock 1.0/Hyperspectral/Updated Canopy Polygons/Updated/7995_Labels_QGIS.shp"
class_lookup_path <- "E:/Git Paint Rock 1.0/Hyperspectral/Models/class_lookup_table.csv"  # CSV created from RF model
test_canopies_path <- "E:/Git Paint Rock 1.0/Hyperspectral/Models/test_canopies.csv"  # ADD: your CSV with TreeID + SpeciesID

# --- Load lookup table created from RF model ---
class_lookup <- read.csv(class_lookup_path, stringsAsFactors = FALSE)

# --- Load test canopy list ---
test_canopies <- read.csv(test_canopies_path, stringsAsFactors = FALSE)

# --- Load classified raster ---
classified_raster_brick <- brick(classified_raster_path)
classified_layer <- classified_raster_brick[[1]]

# --- Load shapefile ---
shapes <- st_read(shapefile_path)

# --- Extract Species from Canopies field ---
shapes <- shapes %>%
  mutate(Species = sub(".*_(\\w+)$", "\\1", Canopies))

# --- Filter shapefile to include only TreeIDs in test_canopies ---
# Extract numeric TreeID from Canopies field (e.g., "123456_SPECIES" -> 123456)
shapes <- shapes %>%
  mutate(TreeID = as.integer(sub("^(\\d+)_.*$", "\\1", Canopies)))

# Keep only test polygons (those not used in training)
shapes <- shapes %>%
  semi_join(test_canopies, by = "TreeID")

# --- Join with class_lookup to assign consistent Species_IDs ---
shapes <- shapes %>%
  left_join(class_lookup, by = c("Species" = "class_name")) %>%
  rename(Species_ID = class_id)

# --- Warn if any species not in lookup ---
if (any(is.na(shapes$Species_ID))) {
  warning("Some species in shapefile were not found in the class lookup table:")
  print(unique(shapes$Species[is.na(shapes$Species_ID)]))
}

# --- Reproject shapefile to match raster CRS ---
if (st_crs(shapes) != st_crs(classified_layer)) {
  shapes <- st_transform(shapes, st_crs(classified_layer))
}

# --- Rasterize filtered shapefile polygons ---
species_raster <- fasterize(shapes, raster = classified_layer, field = "Species_ID")

# --- Extract predicted and actual class values under polygons ---
predicted_vals <- raster::extract(classified_layer, shapes)
actual_vals <- raster::extract(species_raster, shapes)

# --- Flatten, clean, compare ---
predicted <- unlist(predicted_vals)
actual <- unlist(actual_vals)

valid_idx <- which(!is.na(predicted) & !is.na(actual))
predicted <- predicted[valid_idx]
actual <- actual[valid_idx]

# --- Match factor levels with class_lookup ---
predicted <- factor(predicted, levels = class_lookup$class_id)
actual <- factor(actual, levels = class_lookup$class_id)

# --- Create readable labels for confusion matrix ---
class_labels <- setNames(class_lookup$class_name, class_lookup$class_id)

# --- Confusion Matrix ---
conf_mat <- confusionMatrix(predicted, actual)

# --- Print ---
print(conf_mat)

cat("\nConfusion Matrix with Species Labels:\n")
rownames(conf_mat$table) <- class_labels[rownames(conf_mat$table)]
colnames(conf_mat$table) <- class_labels[colnames(conf_mat$table)]
print(conf_mat$table)

cat("\nSpecies ↔ Class ID Lookup Table:\n")
print(class_lookup)

