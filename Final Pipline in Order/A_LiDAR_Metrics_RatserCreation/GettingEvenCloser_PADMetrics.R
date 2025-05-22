
# Set working directory for output
setwd("E:/Git Paint Rock 1.0/Output/LiDAR/Transmittance_Leafoff/")

# Load required libraries
library(AMAPVox)
library(data.table)
library(terra)
library(raster)
library(beepr)

# Step 1: Get file paths for all .vox chunks
chunkFiles <- list.files(
  "E:/Updated LiDAR/Transmittance_Voxels/AMAPVox_batch_results_Transmittance_LeafOff/",
  pattern = "\\.vox$", full.names = TRUE
)

# Step 2: Read and shift voxel chunks to global coordinates
chunkList <- lapply(chunkFiles, function(file) {
  vox <- readVoxelSpace(file)
  if (!is.null(vox@data) && nrow(vox@data) > 0) {
    vox@data <- vox@data[!is.na(ground_distance) & ground_distance < 100]
    origin <- vox@header$mincorner
    vox@data[, i := i + round(origin['x'])]
    vox@data[, j := j + round(origin['y'])]
    vox@data[, k := k + round(origin['z'])]
  }
  return(vox)
})

# Step 3: Combine all voxel data into one data.table
combinedData <- rbindlist(lapply(chunkList, function(x) x@data), use.names = TRUE, fill = TRUE)

# Use first voxel file as template
combinedChunk <- chunkList[[1]]
combinedChunk@data <- combinedData

# Step 4: Correct spatial extent in header
voxelSize <- combinedChunk@header$voxel.size

# Compute real-world coordinates
combinedData[, x := i * voxelSize["x"]]
combinedData[, y := j * voxelSize["y"]]
combinedData[, z := k * voxelSize["z"]]

mincorner <- c(
  x = min(combinedData$x, na.rm = TRUE),
  y = min(combinedData$y, na.rm = TRUE),
  z = min(combinedData$z, na.rm = TRUE)
)
maxcorner <- c(
  x = max(combinedData$x, na.rm = TRUE) + voxelSize["x"],
  y = max(combinedData$y, na.rm = TRUE) + voxelSize["y"],
  z = max(combinedData$z, na.rm = TRUE) + voxelSize["z"]
)

combinedChunk@header$mincorner <- mincorner
combinedChunk@header$maxcorner <- maxcorner

# Step 5: Recalculate voxel indices starting from new origin
combinedChunk@data[, i := round((x - mincorner["x"]) / voxelSize["x"])]
combinedChunk@data[, j := round((y - mincorner["y"]) / voxelSize["y"])]
combinedChunk@data[, k := round((z - mincorner["z"]) / voxelSize["z"])]

# Step 6: Remove butterflies
suppressWarnings(rm(btf))
btf <- tryCatch({butterfly(combinedChunk)}, error = function(e) NULL)
if (!is.null(btf)) clear(combinedChunk, btf)

# Step 7: Compute PAD (Plant Area Density)
pad <- plantAreaDensity(combinedChunk, pulse.min = 2)
combinedChunk@data <- merge(combinedChunk@data, pad, by = c("i", "j", "k"))

# Step 8: Define vertical height bins (e.g., 0–5 m, 5–10 m...)
heightMin <- 0
heightMax <- 50
heightBin <- 5
heightIntervals <- seq(heightMin, heightMax, heightBin)

heightLayerInfo <- data.frame(
  layerName = paste0("PAD_", heightIntervals[-length(heightIntervals)], "_", heightIntervals[-1]),
  minHt = heightIntervals[-length(heightIntervals)],
  maxHt = heightIntervals[-1]
)

# Step 9: Create and store PAD rasters by height bin
for (j in 1:nrow(heightLayerInfo)) {
  minVal <- heightLayerInfo$minHt[j]
  maxVal <- minVal + 1
  
  # First 1-m slice
  PAD_j <- toRaster(
    combinedChunk,
    combinedChunk@data[ground_distance > minVal & ground_distance <= maxVal, .(i, j, pad_transmittance)]
  )
  
  # Add the rest of the 1-m slices up to 5-m bin
  for (k in 1:(heightBin - 1)) {
    minVal <- minVal + 1
    maxVal <- maxVal + 1
    PAD_k <- toRaster(
      combinedChunk,
      combinedChunk@data[ground_distance > minVal & ground_distance <= maxVal, .(i, j, pad_transmittance)]
    )
    PAD_j <- PAD_j + PAD_k
  }
  
  assign(heightLayerInfo$layerName[j], PAD_j)
}

# Step 10: Write rasters to disk
for (i in 1:nrow(heightLayerInfo)) {
  writeRaster(
    get(heightLayerInfo$layerName[i]),
    paste0(heightLayerInfo$layerName[i], "_leafOff_transmittance.tif"),
    overwrite = TRUE
  )
}

# Notify completion
beep()

