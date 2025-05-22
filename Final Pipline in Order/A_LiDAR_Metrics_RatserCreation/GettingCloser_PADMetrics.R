



setwd("E:/Git Paint Rock 1.0/Output/LiDAR/Transmittance_Leafoff/")

library(AMAPVox); library(terra); library(raster)

# get file paths for chunk files
chunkFiles <- 
  list.files("E:/Updated LiDAR/Transmittance_Voxels/AMAPVox_batch_results_Transmittance_LeafOff/", full.names = T)


chunkList <- lapply(chunkFiles, function(file) {
  vox <- readVoxelSpace(file)
  vox@data <- vox@data[!is.na(vox@data$ground_distance) & vox@data$ground_distance < 100]
  return(vox)
})

for (i in seq_along(chunkList)) {
  origin <- chunkList[[i]]@header$mincorner
  chunkList[[i]]@data[, i := i + round(origin['x'])]
  chunkList[[i]]@data[, j := j + round(origin['y'])]
  chunkList[[i]]@data[, k := k + round(origin['z'])]
}

combinedData <- rbindlist(lapply(chunkList, function(x) x@data), use.names = TRUE, fill = TRUE)
combinedChunk <- chunkList[[1]]
combinedChunk@data <- combinedData



# Height bins
heightMin <- 0
heightMax <- 50
heightBin <- 5
heightIntervals <- seq(heightMin, heightMax, heightBin)

# Height layer metadata
heightLayerInfo <- data.frame(
  layerName = paste0("PAD_", heightIntervals[-length(heightIntervals)], "_", heightIntervals[-1]),
  minHt = heightIntervals[-length(heightIntervals)],
  maxHt = heightIntervals[-1]
)

# Remove butterflies
suppressWarnings(rm(btf))
btf <- tryCatch({butterfly(combinedChunk)}, error = function(e) NULL)
if (!is.null(btf)) clear(combinedChunk, btf)

# Compute PAD
pad <- plantAreaDensity(combinedChunk, pulse.min = 2)
beep()

# Merge PAD values back into voxel data
combinedChunk@data <- merge(combinedChunk@data, pad, by = c("i", "j", "k"))

# Initialize PAD rasters
for (j in 1:nrow(heightLayerInfo)) {
  minVal <- heightLayerInfo$minHt[j]
  maxVal <- minVal + 1
  
  # Initial 1-m slice
  PAD_j <- toRaster(combinedChunk, combinedChunk@data[ground_distance > minVal & ground_distance <= maxVal, .(i, j, pad_transmittance)])
  
  # Stack up remaining 1-m slices to form the 5-m bin
  for (k in 1:(heightBin - 1)) {
    minVal <- minVal + 1
    maxVal <- maxVal + 1
    PAD_k <- toRaster(combinedChunk, combinedChunk@data[ground_distance > minVal & ground_distance <= maxVal, .(i, j, pad_transmittance)])
    PAD_j <- PAD_j + PAD_k
  }
  
  # Assign raster to environment
  assign(heightLayerInfo$layerName[j], PAD_j)
}

# Save output rasters
for (i in 1:nrow(heightLayerInfo)) {
  writeRaster(get(heightLayerInfo$layerName[i]),
              paste0(heightLayerInfo$layerName[i], "_leafOff_transmittance.tif"),
              overwrite = TRUE)
}

plot(combinedChunk)
