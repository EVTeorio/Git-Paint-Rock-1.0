

library(lidR)
library(dplyr)
library(tidyr)
library(terra)
beep()


# ---- STEP 1: Load Leaf-On and Leaf-Off LAZ Files ----
leaf_on  <- readLAS("E:/Updated LiDAR/PaintRock_20ha_leafOn_class.laz")
leaf_off <- readLAS("E:/Updated LiDAR/PaintRock_20ha_leafOff_class.laz")

# ---- STEP 2: DTM from leaf-off and normalize both ----
dtm <- rasterize_terrain(leaf_off, res = 1.0, algorithm = knnidw())
leaf_on <- normalize_height(leaf_on, dtm)
leaf_off <- normalize_height(leaf_off, dtm)

# ---- STEP 3: Filter valid points and canopy stratum (e.g., 10–35m) ----
canopy_filter <- function(las) {
  las <- filter_poi(las, !is.na(Z))
  filter_poi(las, Z >= 20 & Z <= 35)
}

leaf_on_canopy <- canopy_filter(leaf_on)
leaf_off_canopy <- canopy_filter(leaf_off)

# ---- STEP 4: Compute voxel occupancy for both datasets ----
voxel_res <- 1  # 1 m³ voxels

voxel_metrics_on  <- voxel_metrics(leaf_on_canopy, ~length(Z), res = voxel_res)
voxel_metrics_off <- voxel_metrics(leaf_off_canopy, ~length(Z), res = voxel_res)

# Convert to data frames
vox_on_df <- as.data.frame(voxel_metrics_on)
vox_off_df <- as.data.frame(voxel_metrics_off)

# Label occupied voxels
vox_on_df$occupied <- vox_on_df$V1 > 0
vox_off_df$occupied <- vox_off_df$V1 > 0

# ---- STEP 5: Aggregate occupied voxels by XY (i.e., vertical column density) ----
agg_on <- aggregate(occupied ~ X + Y, data = vox_on_df, FUN = sum)
agg_off <- aggregate(occupied ~ X + Y, data = vox_off_df, FUN = sum)

# Merge and compute difference
density_df <- full_join(agg_on, agg_off, by = c("X", "Y"), suffix = c("_on", "_off")) %>%
  replace_na(list(occupied_on = 0, occupied_off = 0)) %>%
  mutate(
    norm_on = occupied_on / 25,    # Adjust denominator based on vertical range
    norm_off = occupied_off / 25,
    som = norm_on - norm_off       # Seasonal Occupancy Metric
  )

# ---- STEP 6: Rasterize SOM output ----
som_raster <- rast(x = density_df, type = "xyz", crs = crs(leaf_on))  # assumes CRS is inherited
plot(som_raster)

# Save as GeoTIFF
writeRaster(som_raster, "seasonal_voxel_occupancy.tif", overwrite = TRUE)

