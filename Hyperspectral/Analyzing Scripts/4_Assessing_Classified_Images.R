

# Load required libraries
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(caret)
library(fasterize)

# --- File paths (edit these!) ---
classified_raster_path <- "path_to_your_classified_image.envi"  # ENVI format (ENVI .hdr should be present)
shapefile_path <- "E:/Git Paint Rock 1.0/Hyperspectral/Updated Canopy Polygons/Updated/11892_Labels_QGIS.prj"

# --- Load the classified image as a RasterBrick (ENVI format) ---
classified_raster_brick <- brick(classified_raster_path)

# If your classification result is a single band, this will just be a 1-layer RasterBrick.
# Otherwise, select the band with the class predictions (usually the first one).
classified_layer <- classified_raster_brick[[1]]

# --- Load the canopy shapefile with labeled polygons ---
shapes <- st_read(shapefile_path)

# --- Extract species codes from "Canopies" field (e.g., "123456_SPECIES" -> "SPECIES") ---
shapes <- shapes %>%
  mutate(Species = sub(".*_(\\w+)$", "\\1", Canopies))

# --- Reproject shapefile to match raster CRS, if needed ---
if (st_crs(shapes) != st_crs(classified_layer)) {
  shapes <- st_transform(shapes, st_crs(classified_layer))
}

# --- Rasterize shapefile polygons using Species as field ---
species_raster <- fasterize(shapes, raster = classified_layer, field = "Species")

# --- Extract predicted and actual class labels ---
# Extract predicted classes from classified image under the polygons
predicted_vals <- extract(classified_layer, shapes)

# Extract actual species labels from rasterized shapefile
actual_vals <- extract(species_raster, shapes)

# Flatten the lists to vectors
predicted <- unlist(predicted_vals)
actual <- unlist(actual_vals)

# Filter out NA values
valid_idx <- which(!is.na(predicted) & !is.na(actual))
predicted <- predicted[valid_idx]
actual <- actual[valid_idx]

# Convert to factors with same levels
predicted <- factor(predicted)
actual <- factor(actual, levels = levels(predicted))  # align levels for confusionMatrix

# --- Compute Confusion Matrix ---
conf_mat <- confusionMatrix(predicted, actual)

# --- Print results ---
print(conf_mat)
