

install.packages("raster")

library(AMAPVox); library(terra);
library(raster)
library(beepr)
library(data.table)

# get file paths for chunk files
chunkFiles <- 
  list.files("E:/Updated LiDAR/Transmittance_Voxels/AMAPVox_batch_results_Transmittance_LeafOff/", full.names = T)


chunkList <- lapply(chunkFiles, function(file) {
  vox <- readVoxelSpace(file)
  vox@data <- vox@data[!is.na(vox@data$ground_distance) & vox@data$ground_distance < 100]
  return(vox)
})
beep()
for (i in seq_along(chunkList)) {
  origin <- chunkList[[i]]@header$mincorner
  chunkList[[i]]@data[, i := i + round(origin['x'])]
  chunkList[[i]]@data[, j := j + round(origin['y'])]
  chunkList[[i]]@data[, k := k + round(origin['z'])]
}

combinedData <- rbindlist(lapply(chunkList, function(x) x@data), use.names = TRUE, fill = TRUE)
combinedChunk <- chunkList[[1]]
combinedChunk@data <- combinedData

data <- combinedChunk@data

# Compute transmittance and PAD
data[, PadBV_Estimated := -log(transmittance) / lMeanTotal]
data[!is.finite(PadBV_Estimated), PadBV_Estimated := NA]

# Normalize height to meters above ground
data[, height_bin := floor(ground_distance)]  # round down to nearest meter

# Filter valid PAD values
valid_data <- data[!is.na(PadBV_Estimated) & !is.na(height_bin)]

# stratafied PAD 
band_starts <- seq(0, 40, by = 5)  
height_bands <- lapply(band_starts, function(start) start:(start + 4))
names(height_bands) <- paste0("band_", band_starts, "_", band_starts + 5)

# Initialize list to hold rasters
raster_list <- list()

# Loop over bands to compute and rasterize
for (band_name in names(height_bands)) {
  band_range <- height_bands[[band_name]]
  
  band_data <- valid_data[height_bin %in% band_range]
  
  pad_2d <- band_data[, .(PAD_sum = sum(PadBV_Estimated, na.rm = TRUE)), by = .(i, j)]
  
  # Create raster
  r <- rasterFromXYZ(pad_2d[, .(x = i, y = j, z = PAD_sum)])
  raster_list[[band_name]] <- r
  
  # Plot with base R
  plot(r, main = paste("PAD in", min(band_range), "-", max(band_range) + 1, "m Band"),
       col =  terrain.colors(6),
       zlim = c(.1, 6))
}

plot(r)
################################################################################

# Ensure all rasters have the same extent and resolution by resampling to the reference raster
reference_raster <- raster_list[[1]]  

# Resample all rasters to match the reference raster's extent and resolution
resampled_rasters <- lapply(raster_list, function(r) {
  resample(r, reference_raster, method = "bilinear")  
})

# Create raster stack from resampled rasters
raster_stack <- stack(resampled_rasters)

# Plot the raster stack (first 4 bands)
plot(raster_stack, main = "1m Resolution PAD Raster Stack", col = terrain.colors(100))

# Optional: Save raster stack to GeoTIFF
writeRaster(raster_stack, "E:/Git Paint Rock 1.0/Output/LiDAR/PAD_raster_stack.tif", format = "GTiff", overwrite = TRUE)

