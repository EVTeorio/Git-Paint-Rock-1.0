
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

# Set the directory path
path <- "E:/Final_Rasters/Canopies_VI_Leafon/"
output_path <- "E:/Final_Rasters/VI_Leafon.csv"

# Define the new column names for the first 95 bands
new_band_names <- c(
  "Boochs", "Boochs2", "CARI", "Carter", "Carter2", "Carter3", "Carter4", "Carter5", 
  "Carter6", "CI", "CI2", "ClAInt", "CRI1", "CRI2", "CRI3", "CRI4", "D1", "D2", "Datt", 
  "Datt2", "Datt3", "Datt4", "Datt5", "Datt6", "DD", "DDn", "DPI", "DWSI4", "EGFN", 
  "EGFR", "EVI", "GDVI2", "GDVI3", "GDVI4", "GI", "Gitelson", "Gitelson2", "GMI1", 
  "GMI2", "GreenNDVI", "Maccioni", "MCARI", "MCARIOSAVI", "MCARI2", "MCARI2OSAVI2", 
  "mND705", "mNDVI", "MPRI", "MSAVI", "mSR", "mSR2", "mSR705", "MTCI", "MTVI", "NDVI", 
  "NDVI2", "NDVI3", "NPCI", "OSAVI", "OSAVI2", "PARS", "PRI", "PRICI2", "PRInorm", 
  "PSND", "PSRI", "PSSR", "RDVI", "REPLE", "REPLi", "SAVI", "SIPI", "SPVI", "SR", 
  "SR1", "SR2", "SR3", "SR4", "SR5", "SR6", "SR7", "SR8", "SRPI", "SumDr1", "SumDr2", 
  "TCARI", "TCARIOSAVI", "TCARI2", "TCARI2OSAVI2", "TGI", "TVI", "Vogelmann", 
  "Vogelmann2", "Vogelmann3", "Vogelmann4"
)

# Define the function
extract_spectral_data <- function(path) {
  allfiles <- list.files(path) 
  imgs <- subset(allfiles, grepl("\\.ENVI$", allfiles))
  
  all_spectral_data <- list()
  
  for (x in seq_along(imgs)) {
    img_path <- file.path(path, imgs[x])
    
    img <- brick(img_path)
    spectral_data <- as.data.frame(as.matrix(img))
    
    # Check if there are at least 95 bands before renaming
    if (ncol(spectral_data) < length(new_band_names)) {
      warning(paste("File", imgs[x], "has fewer than 95 bands — skipping."))
      next
    }
    
    # Rename first 95 columns
    colnames(spectral_data)[1:95] <- new_band_names
    
    # Metadata extraction
    imgs_names <- str_match(imgs[x], "(.*)\\.ENVI")[1, 2]
    TrID <- str_split(imgs_names, "_")[[1]]
    
    tile_number <- TrID[1]
    tree_id     <- TrID[2]
    species_id  <- TrID[3]
    
    TrID_df <- data.frame(TileNumber = tile_number, SpeciesID = species_id, TreeID = tree_id)
    
    spectral_data <- cbind(TrID_df, spectral_data)
    all_spectral_data[[x]] <- spectral_data
  }
  
  # Combine and write
  final_df <- do.call(rbind, all_spectral_data)
  write.csv(final_df, file.path(output_path), row.names = FALSE)
  
  return(final_df)
}

# Run the function
spectral_df <- extract_spectral_data(path)
beep(3)



