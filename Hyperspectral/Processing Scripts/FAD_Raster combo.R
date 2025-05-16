


beep(3)
# Load the foliage density raster
fad_raster <- brick("E:/Git Paint Rock 1.0/Output/LiDAR/PAD_raster_stack.tif")
fad_raster <- projectRaster(fad_raster, crs = "NAD83(CSRS) / UTM zone 16N")

# Function to convert hyperspectral raster files into data frames and attach foliage density
convert_raster_to_dfs <- function(path) {
  # List all .ENVI files in the path
  allfiles <- list.files(path)
  envi_files <- allfiles[!grepl("\\.hdr$|\\.aux$|\\.enp$|\\.sta$", allfiles)]
  
  for (file in envi_files) {
    # Full path to the raster
    img_path <- file.path(path, file)
    
    # Load raster brick
    raster_data <- brick(input_path)
    
    # Get coordinates of each pixel
    coords <- coordinates(raster_data)
    
    # Convert raster values to a data frame
    spectral_values <- as.data.frame(as.matrix(raster_data))
    
    # Combine coordinates and spectral values
    coor_df <- cbind(data.frame(x = coords[, 1], y = coords[, 2]), spectral_values)
    
    # Extract foliage density values at these coordinates
    fad_values <- extract(fad_raster, coords)
    
    # If FAD has multiple layers, give them names
    if (nlayers(fad_raster) > 1) {
      colnames(fad_values) <- paste0("FAD_layer_", 1:ncol(fad_values))
    } else {
      fad_values <- data.frame(FAD = fad_values)
    }
    
    # Append FAD data to the final dataframe
    final_df <- cbind(final_df, fad_values)
    
    # Optionally save to CSV (customize filename)
    #output_filename <- paste0("E:/Output/CSV/", tools::file_path_sans_ext(file), "_with_FAD.csv")
    #write.csv(final_df, output_filename, row.names = FALSE)
  }
}