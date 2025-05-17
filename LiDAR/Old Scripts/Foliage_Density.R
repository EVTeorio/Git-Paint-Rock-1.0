


library(lidR)
library(rlas)
library(terra)
library(beepr)
beep()

# 1. Load the raw LAZ file
las <- readLAS("E:/Updated LiDAR/PaintRock_20ha_leafOn_Subset.laz")

# 2. Create a DTM (Digital Terrain Model) using ground points
# This step does NOT rely on classification; if classification is unreliable,
# you can use ground filtering algorithms like pmf() or csf().
dtm <- rasterize_terrain(las, res = 1, algorithm = knnidw())
plot(laz)
# 3. Normalize heights: subtract ground elevation (Z above ground)
las_norm <- normalize_height(las, dtm)

summary(las_norm$Z)
# Optionally remove points with NA height (e.g., areas with no DTM)
las_norm <- filter_poi(las_norm, !is.na(Z))

# 4. Filter points between 15m and 30m above ground
las_canopy <- filter_poi(las_norm, Z >= 10 & Z <= 35)
npoints(las_canopy)


# 5. Define voxel size (e.g., 1m³ voxels)
voxel_res <- 1

# 6. Compute voxel metrics in canopy layer
voxels <- voxel_metrics(las_canopy, ~length(Z), res = voxel_res)
voxels_df <- as.data.frame(voxels)

# 7. Mark occupied voxels
voxels_df$occupied <- voxels_df$V1 > 0

# 8. Aggregate: number of occupied voxels per X-Y column (foliage density in 15–30m band)
foliage_density_df <- aggregate(occupied ~ X + Y, data = voxels_df, FUN = sum)

# 9. Optionally normalize: max possible vertical bins in 15–30m band = (30-15)/voxel_res = 15
foliage_density_df$norm_density <- foliage_density_df$occupied / 15

# 10. Convert to raster
r <- rast(foliage_density_df[, c("X", "Y", "occupied")], type = "xyz")
names(r) <- "foliage_density"

# 11. Save to file (optional)
writeRaster(r, "E:/Updated LiDAR/foliage_density_15_30m.tif", overwrite = TRUE)

# 12. Plot
plot(r, main = "Foliage Density (20–40 m AGL)")
