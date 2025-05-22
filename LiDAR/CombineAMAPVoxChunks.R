#this file:  E:/Git Paint Rock 1.0/LiDAR/CombineAMAPVoxChunks.R


setwd("E:/Git Paint Rock 1.0/Output/LiDAR/Transmittance_Leafoff/")

library(AMAPVox); library(terra); library(raster)

# get file paths for chunk files
chunkFiles <- 
  list.files("E:/Updated LiDAR/Transmittance_Voxels/AMAPVox_batch_results_Transmittance_LeafOff/", full.names = T)

# set height intervals of interest
heightMin <- 0
heightMax <- 50
heightBin <- 5
heightIntervals <- seq(heightMin, heightMax, heightBin)

# make data frame of height layer info/names
heightLayerInfo <- data.frame(layerName = paste0("PAD_",heightIntervals[-length(heightIntervals)],"_",heightIntervals[-1]),
                              minHt = heightIntervals[-length(heightIntervals)],
                              maxHt = heightIntervals[-1])

# loop through files and convert to PAI rasters
for(i in 1:length(chunkFiles)){
  
  # read file
  chunk <- readVoxelSpace(chunkFiles[i])
  
  # only continue if there are data with valid ground points in the AOI
  if(min(chunk@data$ground_distance)<100){
  
    # identify butterflies
    suppressWarnings(rm(btf))
    btf <- tryCatch(
      {butterfly(chunk)},
      error = function(e) {return(NULL)}
      )
      
    # clear butterflies
    if(!is.null(btf)){clear(chunk, btf)}
    
    # compute PAD
    pad <- plantAreaDensity(chunk, pulse.min = 2)
    
    # merge pad variables into voxel space
    chunk@data <- merge(chunk@data, pad, by = c("i", "j", "k"))
    
    # print PAD variable names
    # grep("^pad", names(chunk), value = TRUE)
    
    # make new rasters if they don't yet exist
    if(!exists(heightLayerInfo$layerName[1])){
    
      for(j in 1:nrow(heightLayerInfo)){
        minVal <- heightLayerInfo$minHt[j]
        maxVal <- heightLayerInfo$minHt[j] +1
        
        PAD_j <-  toRaster(chunk, chunk@data[ground_distance > minVal & ground_distance <= maxVal , .(i, j, pad_transmittance)])
        
        for(k in 1:(heightBin-1)){
          minVal <- minVal+k
          maxVal <- maxVal+k
          PAD_k <-  toRaster(chunk, chunk@data[ground_distance > minVal & ground_distance <= maxVal , .(i, j, pad_transmittance)])
          PAD_j <- PAD_j + PAD_k
        }
        
      assign(heightLayerInfo$layerName[j], PAD_j)
  
      }
    }
    
    # merge rasters if this is not the first raster
    if(exists(heightLayerInfo$layerName[1])){
      
      for(j in 1:nrow(heightLayerInfo)){
        minVal <- heightLayerInfo$minHt[j]
        maxVal <- heightLayerInfo$minHt[j] +1
        
        PAD_j <-  toRaster(chunk, chunk@data[ground_distance > minVal & ground_distance <= maxVal , .(i, j, pad_transmittance)])
        
        # add 1-m layers within the height range
        for(k in 1:(heightBin-1)){
          minVal <- minVal+k
          maxVal <- maxVal+k
          PAD_k <-  toRaster(chunk, chunk@data[ground_distance > minVal & ground_distance <= maxVal , .(i, j, pad_transmittance)])
          PAD_j <- PAD_j + PAD_k
        }
        
        # rename with temporary value
        assign(paste0(heightLayerInfo$layerName[j],"_j"), PAD_j)
        
        # merge rasters
        mergedRast <- merge(get(heightLayerInfo$layerName[j]),
                            get(paste0(heightLayerInfo$layerName[j],"_j")))
        
        # rename merged raster
        assign(heightLayerInfo$layerName[j], mergedRast)
        
        
      }
      
    }

  }
}

beep()

# save raster files
for(i in 1:nrow(heightLayerInfo)){
  writeRaster(get(heightLayerInfo$layerName[i]),
              paste0(heightLayerInfo$layerName[i],"_leafOff_transmittance.tif")) 
}
################################################################################

################################################################################
# Set your folder path
folder_path <- "E:/LeafOff_Rasters/"

# List all raster files (e.g., .tif files)
raster_files <- list.files(path = folder_path, pattern = "\\.tif$", full.names = TRUE)

# Read and stack the rasters
raster_stack <- rast(raster_files)

# Assuming raster_stack is a SpatRaster object from terra
# Define a color palette: red for low, blue for high values
myColors <- colorRampPalette(c("yellow","red", "green", "blue"))(100)

# Plot using custom color ramp
plot(raster_stack, col = myColors)

plot(raster_stack)
plot(mergedRast)
