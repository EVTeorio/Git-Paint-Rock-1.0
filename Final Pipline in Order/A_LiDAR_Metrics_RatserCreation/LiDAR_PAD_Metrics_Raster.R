setwd("E:/LeafOn_Rasters/")

library(AMAPVox); library(terra)

# get file paths for chunk files
chunkFiles <- list.files("E:/Updated LiDAR/Transmittance_Voxels/AMAPVox_batch_results_Transmittance_LeafOn/", full.names = TRUE)

# set height intervals of interest
heightMin <- 0
heightMax <- 50
heightBin <- 5
heightIntervals <- seq(heightMin, heightMax, heightBin)

# make data frame of height layer info/names
heightLayerInfo <- data.frame(
  layerName = paste0("PAD_", heightIntervals[-length(heightIntervals)], "_", heightIntervals[-1]),
  minHt = heightIntervals[-length(heightIntervals)],
  maxHt = heightIntervals[-1]
)

# loop through files and convert to PAI rasters
for (i in seq_along(chunkFiles)) {
  
  # read voxel file
  chunk <- readVoxelSpace(chunkFiles[i])
  
  # continue only if valid ground points exist
  if (min(chunk@data$ground_distance, na.rm = TRUE) < 100) {
    
    # clear butterflies if found
    suppressWarnings(rm(btf))
    btf <- tryCatch(butterfly(chunk), error = function(e) NULL)
    if (!is.null(btf)) clear(chunk, btf)
    
    # compute PAD
    pad <- plantAreaDensity(chunk, pulse.min = 3)
    chunk@data <- merge(chunk@data, pad, by = c("i", "j", "k"))
    
    # create rasters if they don’t exist
    if (!exists(heightLayerInfo$layerName[1])) {
      for (j in seq_len(nrow(heightLayerInfo))) {
        minVal <- heightLayerInfo$minHt[j]
        PAD_j <- toRaster(chunk, chunk@data[ground_distance > minVal & ground_distance <= minVal + 1, .(i, j, pad_transmittance)])
        for (k in 1:(heightBin - 1)) {
          subMin <- minVal + k
          PAD_k <- toRaster(chunk, chunk@data[ground_distance > subMin & ground_distance <= subMin + 1, .(i, j, pad_transmittance)])
          PAD_j <- c(PAD_j, PAD_k)
        }
        nObs_j <- PAD_j
        nObs_j[!is.na(nObs_j)] <- 1
        nObs_raster <- sum(nObs_j, na.rm = TRUE)
        PAD_sum <- sum(PAD_j, na.rm = TRUE)
        PAD_scaled <- PAD_sum * (5 / nObs_raster)
        names(PAD_scaled) <- heightLayerInfo$layerName[j]  # et name
        assign(heightLayerInfo$layerName[j], PAD_scaled)
      }
    } else {
      # merge rasters
      for (j in seq_len(nrow(heightLayerInfo))) {
        minVal <- heightLayerInfo$minHt[j]
        PAD_j <- toRaster(chunk, chunk@data[ground_distance > minVal & ground_distance <= minVal + 1, .(i, j, pad_transmittance)])
        for (k in 1:(heightBin - 1)) {
          subMin <- minVal + k
          PAD_k <- toRaster(chunk, chunk@data[ground_distance > subMin & ground_distance <= subMin + 1, .(i, j, pad_transmittance)])
          PAD_j <- c(PAD_j, PAD_k)
        }
        nObs_j <- PAD_j
        nObs_j[!is.na(nObs_j)] <- 1
        nObs_raster <- sum(nObs_j, na.rm = TRUE)
        PAD_sum <- sum(PAD_j, na.rm = TRUE)
        PAD_scaled <- PAD_sum * (5 / nObs_raster)
        names(PAD_scaled) <- paste0(heightLayerInfo$layerName[j], "_j")  # temporary name
        mergedRast <- merge(get(heightLayerInfo$layerName[j]), PAD_scaled)
        names(mergedRast) <- heightLayerInfo$layerName[j]  # estore original name
        assign(heightLayerInfo$layerName[j], mergedRast)
      }
    }
  }
}

beep()

# save raster files with NA imputed as -1 and correct names
for (i in seq_len(nrow(heightLayerInfo))) {
  r <- get(heightLayerInfo$layerName[i])
  r[is.na(r)] <- -1  # Impute NA with -1
  names(r) <- heightLayerInfo$layerName[i]  # plicitly set layer name
  writeRaster(r,
              filename = paste0(heightLayerInfo$layerName[i], "_leafOn_transmittance.tif"),
              overwrite = TRUE)
}

######Combining layers#####################################################

# Set your folder path
folder_path <- "E:/LeafOn_Rasters/Individual_rasters"

# List all raster files (e.g., .tif files)
raster_files <- list.files(path = folder_path, pattern = "\\.tif$", full.names = TRUE)

# Read and stack the rasters
raster_stack <- rast(raster_files)

# Set names from filenames
names(raster_stack) <- gsub("_leafOn_transmittance.tif", "", basename(raster_files))

plot(raster_stack)

# Optional: Save raster stack to GeoTIFF
writeRaster(raster_stack, "E:/LeafOn_Rasters/LeafOn_stack.tif", overwrite = TRUE)


################################################3
library(terra)


PAD <- rast("D:/LiDAR_Metrics/ALL_Metrics.tif")


plot(PAD[[21]])

