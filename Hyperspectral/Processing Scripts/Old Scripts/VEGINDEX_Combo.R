


setwd("lecospec")
source("Functions/lecospectR.R")

# Load necessary libraries
library(raster)
library(dplyr)
library(tidyr)
library(stringr)
library(spectrolab)
library(RStoolbox)
library(hyperSpec)
library(beepr)
beep()


img_path <- input_path

# Set the input raster folder and output CSV location
input_path <- "E:/HSI_Files_Parsing/"

# Function to convert hyperspectral raster files into data frames
convert_raster_to_dfs <- function(path) {
  # List all .ENVI files in the path
  allfiles <- list.files(path)
  envi_files <- allfiles[!grepl("\\.hdr$|\\.aux$|\\.enp$|\\.sta$", allfiles)]
  
  for (file in envi_files) {
    # Full path to the raster
    img_path <- file.path(path, file)
    
    # Load raster brick
    raster_data <- brick(img_path)
    
    # Get coordinates of each pixel
    coords <- coordinates(raster_data)
    
    # Convert raster values to a data frame
    spectral_values <- as.data.frame(as.matrix(raster_data))
    
    # Combine coordinates and spectral values
    final_df <- cbind(
      data.frame(x = as.character(coords[, 1]), y = as.character(coords[, 2])),
      spectral_values
    )
    
    # Convert the data frame to a format compatible with your vegetation index function
    trees_image_spectra_df <- speclib_to_df(final_df)
    
    # Calculate vegetation indices for the pixels
    trees_image_spectra_VIs <- get_vegetation_indices(trees_image_spectra_df, NULL)
    
    # Ensure x and y are numeric
    VI_df_with_coords <- cbind(
      x = as.numeric(coords[, 1]),
      y = as.numeric(coords[, 2]),
      trees_image_spectra_VIs
    )
    
    # Convert to SpatialPixelsDataFrame
    spdf <- SpatialPixelsDataFrame(points = VI_df_with_coords[, c("x", "y")],
                                   data = VI_df_with_coords[, !(names(VI_df_with_coords) %in% c("x", "y"))],
                                   tolerance = 0.0001)  # Adjust tolerance if needed
    
    # Convert to raster brick
    VI_raster <- brick(spdf)
    
    # Match the original raster's extent, resolution, and CRS
    extent(VI_raster) <- extent(raster_data)
    res(VI_raster) <- res(raster_data)
    crs(VI_raster) <- crs(raster_data)
    
    # Set output filename
    output_name <- file.path("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Vegetaion Indices Images/",
                             paste0(tools::file_path_sans_ext(file), "_VI"))
    
    # Write raster as ENVI format
    writeRaster(VI_raster, filename = output_name, format = "ENVI", overwrite = TRUE)
  }
}
# Run the function
convert_raster_to_dfs(input_path)
