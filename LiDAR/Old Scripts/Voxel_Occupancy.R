


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

# ---- STEP 3: Filter valid points and canopy stratum (e.g., 20–35m) ----
canopy_filter <- function(las) {
  las <- filter_poi(las, !is.na(Z))
  filter_poi(las, Z >= 20 & Z <= 35)
}

leaf_on_canopy <- canopy_filter(leaf_on)
leaf_off_canopy <- canopy_filter(leaf_off)

# ---- STEP 4: Compute voxel presence (volume proxy) ----
voxel_res <- 1  # voxel resolution (1m³)

voxel_on <- voxel_metrics(leaf_on_canopy, ~length(Z), res = voxel_res)
voxel_off <- voxel_metrics(leaf_off_canopy, ~length(Z), res = voxel_res)

# Convert to data frames
vox_on_df <- as.data.frame(voxel_on)
vox_off_df <- as.data.frame(voxel_off)

# Set 'volume' per voxel if occupied
vox_on_df$volume_m3 <- ifelse(vox_on_df$V1 > 0, voxel_res^3, 0)
vox_off_df$volume_m3 <- ifelse(vox_off_df$V1 > 0, voxel_res^3, 0)

# ---- STEP 5: Aggregate volume per XY column ----
agg_vol_on <- vox_on_df %>%
  group_by(X, Y) %>%
  summarise(vol_on_m3 = sum(volume_m3), .groups = "drop")

agg_vol_off <- vox_off_df %>%
  group_by(X, Y) %>%
  summarise(vol_off_m3 = sum(volume_m3), .groups = "drop")

# Merge and compute seasonal difference
volume_df <- full_join(agg_vol_on, agg_vol_off, by = c("X", "Y")) %>%
  replace_na(list(vol_on_m3 = 0, vol_off_m3 = 0)) %>%
  mutate(
    som_volume = vol_on_m3 - vol_off_m3  # seasonal volume change
  )

# ---- STEP 6: Rasterize seasonal volume difference ----
som_vol_raster <- rast(x = volume_df, type = "xyz", crs = crs(leaf_on))
plot(som_vol_raster)

# Save raster
writeRaster(som_vol_raster, "seasonal_voxel_volume.tif", overwrite = TRUE)

