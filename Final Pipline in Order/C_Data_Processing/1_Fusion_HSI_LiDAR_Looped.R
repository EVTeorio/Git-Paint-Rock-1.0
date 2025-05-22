

library(terra)

# Set file paths
vi_dir <- "E:/Vegetaion Indices Images/"
output_dir <- "E:/Final_Rasters/Fusion_VI_ALLmetrics_Images"
lidar_path <- "E:/LiDAR_Metrics/ALL_Metrics.tif"
crs_raster_path <- "E:/Updated LiDAR/PRFPD_CHM_leafOff.tiff"

# Load static rasters
lidar <- rast(lidar_path)
justforcrs <- rast(crs_raster_path)

# Apply CRS to LiDAR
crs(lidar) <- crs(justforcrs)

# Get list of all .envi files in the VI directory
vi_files <- list.files(path = vi_dir, pattern = "\\.envi$", full.names = TRUE)

# Loop through each VI file
for (vi_path in vi_files) {
  # Load VI raster
  vi <- rast(vi_path)
  
  # Reproject lidar to VI raster
  lidar_reproj <- project(lidar, vi)
  
  # Resample lidar to match VI raster
  lidar_aligned <- resample(lidar_reproj, vi, method = "bilinear")
  
  # Combine VI and LiDAR
  combined <- c(vi, lidar_aligned)
  
  # Generate output filename based on VI filename
  vi_filename <- tools::file_path_sans_ext(basename(vi_path))  # Remove extension
  output_path <- file.path(output_dir, paste0(vi_filename, "_ALLmetrics.envi"))
  
  # Write output raster
  writeRaster(combined,
              filename = output_path,
              filetype = "ENVI",
              overwrite = TRUE)
  
  cat("Processed:", vi_filename, "\n")
}
beep(3)
