
library(terra)


LeafOn <- rast("E:/LiDAR_Metrics/Metrics/LeafOff_stack.tif")

# Get current layer names
original_names <- names(LeafOn)

# Append "_on" to each name
names(LeafOn) <- paste0(original_names, "_off")

# (Optional) Save the renamed raster if needed
writeRaster(LeafOn, "E:/LiDAR_Metrics/Metrics/LeafOff_stack1.tif", overwrite=TRUE)

# Set your folder path
folder_path <- "E:/LiDAR_Metrics/Metrics"

# List all raster files (e.g., .tif files)
raster_files <- list.files(path = folder_path, pattern = "\\.tif$", full.names = TRUE)

# Read and stack the rasters
raster_stack <- rast(raster_files)
plot(raster_stack[[20]])
writeRaster(raster_stack, "E:/LiDAR_Metrics/ALL_Metrics.tif", overwrite=TRUE)
