

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
beep(3)


img_path <- input_path

# Set the input raster folder and output CSV location
input_path <- "E:/HSI_Files_Parsing/raw_0_rd_rf_or"

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
    
    # Create a valid R variable name from the file name (remove extension and non-alphanumeric characters)
    df_name <- make.names(tools::file_path_sans_ext(as.character(file)))
    
    # Assign the data frame to the global environment
    assign(df_name, trees_image_spectra_VIs, envir = .GlobalEnv)
  }
}

# Run the function
convert_raster_to_dfs(input_path)
###############################################################################

