
# Load necessary libraries
library(raster)
library(sf)
library(rasterVis)
library(ggplot2)
beep(3)

# 1. Load the classification raster (assumed to have discrete species labels)
raster_path <- "E:/Git Paint Rock 1.0/Output/prediction_raster.tif"
classification_raster <- raster(raster_path)
plot(canopy_sf)
# 2. Define the region of interest (ROI) to subset the image (for example, a bounding box)
# You can specify the extent (xmin, xmax, ymin, ymax) of the area you want to crop
roi_extent <- extent(-86.3055, -86.3052, 34.7721, 34.7723)  # Modify based on your raster coordinates
cropped_raster <- crop(classification_raster, roi_extent)

# 3. Convert the cropped classification raster to polygons (each unique class will be a distinct canopy shape)
canopy_polygons <- rasterToPolygons(cropped_raster, dissolve = TRUE)

# 4. Convert the polygons to an 'sf' object for better handling and visualization
canopy_sf <- st_as_sf(canopy_polygons)

# 5. Derive species labels from the unique class values in the cropped raster
# Get the unique class values from the raster (species IDs)
species_ids <- unique(values(cropped_raster))
species_labels <- paste("Species", species_ids)  # Create labels like "Species 1", "Species 2", etc.

# Assign species labels to the polygons based on the class IDs
canopy_sf$species <- factor(canopy_sf$ID, labels = species_labels)

# 6. Visualize the canopy shapes
ggplot() +
  geom_sf(data = canopy_sf, aes(fill = species), color = "black", size = 0.2) +
  scale_fill_manual(values = rainbow(length(species_labels))) +  # Assign colors to species
  labs(title = "Predicted Canopy Shapes by Species",
       fill = "Species") +
  theme_minimal() +
  theme(legend.position = "right")

# 7. Optionally, save the vectorized canopy shapes as a shapefile for further analysis
st_write(canopy_sf, "E:/Git Paint Rock 1.0/Output/canopy_shapes.shp")
