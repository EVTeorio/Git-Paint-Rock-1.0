
# Load required library
library(terra)

# Step 1: Load rasters
vi <- rast("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Vegetaion Indices Images/raw_11892_rd_rf_or_VI.envi")
lidar <- rast("E:/Git Paint Rock 1.0/Output/LiDAR/Transmittance_Leafoff/PAD_25_30_leafOff_transmittance.tif")
justforcrs <- rast("E:/Updated LiDAR/PRFPD_CHM_leafOff.tiff")

crs(lidar) <- justforcrs
lidar_reproj <- project(lidar, vi)

# Resample LiDAR raster to match VI raster resolution and extent
lidar_aligned <- terra::resample(lidar_reproj, vi, method = "bilinear")  # or use "near" if categorical

combined <- c(vi, lidar_aligned)

# ---- Write combined raster to ENVI format ----
writeRaster(combined,
            filename = "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/combined_output.envi",
            format = "ENVI",
            overwrite = TRUE)





