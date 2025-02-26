
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

# Define the function to extract spectral values and metadata
extract_spectral_data <- function(path) {
  # List all .ENVI files in the given path
  allfiles <- list.files(path) 
  imgs <- subset(allfiles, grepl("\\.ENVI$", allfiles))
  
  # Initialize an empty list to store results
  all_spectral_data <- list()
  
  # Loop over each file to extract spectral data and metadata
  for (x in seq_along(imgs)) {
    # Construct the full path to the image
    img_path <- file.path(path, imgs[x])#trouble shooting
    
    # Use raster to read the ENVI file
    img <- brick(img_path)  # raster::brick() reads multi-band files
    
    # Extract spectral data: we assume it's a multi-band raster
    spectral_data <- as.data.frame(as.matrix(img))  # Convert the raster to a matrix and then to a data frame
    
    # Extract metadata from the filename using str_match (as per your suggestion)
    imgs_names <- str_match(imgs[x], "(.*)\\.ENVI")  # Match the name without the extension
    imgs_names <- imgs_names[1, 2]  # Access the first captured group (the filename part without extension)
    
    # Split metadata from filename using str_split()
    TrID <- str_split(imgs_names, "_")[[1]]  # Split by underscore (_)
    
    # Extract tile number (assuming it's always the first part of the name)
    tile_number <- TrID[1]  # The first part before the first underscore is the tile number
    species_id <- TrID[2]   # The second part is the SpeciesID
    tree_id <- TrID[3]      # The third part is the TreeID
    
    # Convert to a data frame (add tile number, species ID, and tree ID as new columns)
    TrID_df <- data.frame(TileNumber = tile_number, SpeciesID = species_id, TreeID = tree_id)
    
    # Combine spectral data and metadata
    spectral_data <- cbind(TrID_df, spectral_data)
    
    # Add to the list of results
    all_spectral_data[[x]] <- spectral_data
  }
  
  # Combine all the spectral data from the list into a single data frame
  final_df <- do.call(rbind, all_spectral_data)
  
  # Write the combined data to a CSV file
  write.csv(final_df, file.path("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Raw_PixelSpectra_Trial.csv"),
            row.names = FALSE)
  
  return(final_df)
}

# Set the directory path
path <- "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/canopy_spectra/Canopy_Rasters/"

# Call the function to process the data and save it as CSV (this takes a while)
spectral_df <- extract_spectral_data(path)



