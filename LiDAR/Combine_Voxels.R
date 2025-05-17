

# Load required libraries
library(AMAPVox)     # for reading .vox files
library(data.table)  # efficient table handling
library(dplyr)
library(raster)


# Define your voxel folder
vox_path <- "E:/Updated LiDAR/Transmittance_Voxels/AMAPVox_batch_results_Transmittance_LeafOn/"

# List all .vox files
vox_files <- list.files(vox_path, pattern = "\\.vox$", full.names = TRUE)

# Read all VoxelSpace objects
vox_list <- lapply(vox_files, function(f) {
  try(readVoxelSpace(f))  # safely read, returns VoxelSpace S4 object
})

# Extract and combine the data.table slot from each voxel object
vox_data_list <- lapply(vox_list, function(v) v@data)

# Combine them using voxel indices as keys
# We'll sum numeric columns for each voxel (i, j, k)
combined_vox_data <- rbindlist(vox_data_list) %>%
  .[, lapply(.SD, function(x) if (is.numeric(x)) sum(x, na.rm = TRUE) else x[1]), 
    by = .(i, j, k)]

# Copy header and metadata from first voxel (assuming identical structure)
combined_vox <- vox_list[[1]]
combined_vox@data <- combined_vox_data

# Optional: Save to new .vox file
output_path <- file.path(vox_path, "combined_voxel_leafon.vox")
writeVoxelSpace(combined_vox, output_path)

######################################################################

# Compute Plant Area Density (returns data.table, not VoxelSpace)
pad_data <- plantAreaDensity(combined_vox, pulse.min = 2)

# Extract voxel grid info from the original combined voxel object
voxel_size <- combined_vox@header$`voxel.size`
min_corner <- combined_vox@header$`mincorner`
dims <- combined_vox@header$dim

# Create raster layers by vertical layer (k)
library(raster)
pad_stack <- stack()
for (z in 0:(dims["z"] - 1)) {
  slice <- pad_data[k == z, .(i, j, pad_transmittance)]
  mat <- matrix(NA, nrow = dims["y"], ncol = dims["x"])
  
  for (row in 1:nrow(slice)) {
    i <- slice$i[row] + 1  # R is 1-based
    j <- slice$j[row] + 1
    mat[j, i] <- slice$pad_transmittance[row]
  }
  
  r <- raster(mat,
              xmn = min_corner["x"],
              xmx = min_corner["x"] + dims["x"] * voxel_size["x"],
              ymn = min_corner["y"],
              ymx = min_corner["y"] + dims["y"] * voxel_size["y"],
              crs = NA)
  names(r) <- paste0("PAD_z", z)
  pad_stack <- addLayer(pad_stack, r)
}

# OPTIONAL: Save raster stack to disk
writeRaster(pad_stack, filename = file.path(vox_path, "PAD_stack.tif"),
            format = "GTiff", overwrite = TRUE)
