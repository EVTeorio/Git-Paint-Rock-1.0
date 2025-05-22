


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



# Set the input raster folder and output CSV location
img_path <- "E:/HSI_Files_Parsing/raw_5626_rd_rf_or"

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
    
# Calculate vegetation indices for the pixels 1 hour per 1.4GB on 16 GB RAM 14 thread processor
trees_image_spectra_VIs <- get_vegetation_indices(trees_image_spectra_df, NULL)
beep()

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
output_name <- file.path("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Vegetaion Indices Images/raw_5626_rd_rf_or_VI")

# Write raster as ENVI format
writeRaster(VI_raster, filename = output_name, format = "ENVI", overwrite = TRUE)
beep(3)
