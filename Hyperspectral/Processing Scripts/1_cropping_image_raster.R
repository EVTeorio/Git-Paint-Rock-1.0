
library(terra)
library(raster)
library(sf)

# Define file paths
HSI_dir <- "E:/HSI_Files_Parsing"  # Directory containing hyperspectral images
canopies_path <- "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Labeling/Label Shapefiles"  # Path to shapefiles

# List all files in the hyperspectral image directory (without extensions)
allfiles <- list.files(HSI_dir)

# Exclude files that are not raster files
imgs <- allfiles[!grepl("\\.hdr$|\\.aux$|\\.enp$|\\.sta$", allfiles)]  # Change this to match your file extensions

# Extract the image numbers from the filenames (assuming the image number is part of the filename)
img_numbers <- gsub("\\D", "", imgs)  # Extract numbers only from image filenames

# List all shapefiles in the shapefile directory
canopies_sites <- list.files(canopies_path, full.names = TRUE, pattern = "*.shp$")  # List of shapefiles

# Extract the image numbers from the shapefile filenames
shapefile_numbers <- gsub("\\D", "", basename(canopies_sites))  # Extract numbers only from shapefile filenames

# Match hyperspectral images to shapefiles based on image number
matched_files <- intersect(img_numbers, shapefile_numbers)

# Check if there are any unmatched files
if (length(matched_files) == 0) {
  stop("No matching image numbers between hyperspectral images and shapefiles.")
}

# Process each matched image and its corresponding shapefile
lapply(matched_files, function(img_number) {
  
  # Get the corresponding hyperspectral image and shapefile based on the image number
  img_idx <- which(img_numbers == img_number)  # Ensure only one match is found
  img_path <- file.path(HSI_dir, imgs[img_idx])  # Full path to the image
  canopy_path <- canopies_sites[shapefile_numbers == img_number]  # Full path to the shapefile
  
  # Check if the image path exists before proceeding
  if (!file.exists(img_path)) {
    stop(paste("Image file does not exist:", img_path))
  }
  
  # Load the hyperspectral image
  tst_img <- terra::rast(img_path)
  tst_names <- names(tst_img)  # Get the band names
  
  # Load the corresponding canopy shapefile
  tst_quads <- terra::vect(canopy_path)
  
  # Process each polygon in the shapefile
  lapply(1:length(tst_quads), function(i) {
    
    # Get the polygon
    canopy_polygon <- tst_quads[i]
    
    # Get the name of the polygon (assuming it's stored in a field called "Canopies")
    polygon_name <- canopy_polygon$Canopies  # Adjust if needed to the correct field name in your shapefile
    
    # If the polygon name is NULL or missing, use a default name
    if (is.null(polygon_name) | is.na(polygon_name)) {
      polygon_name <- paste0(i)  # Default name if 'Canopies' is missing
    }
    
    # Add the hyperspectral image number to the polygon name
    polygon_name <- paste0(img_number, "_", polygon_name)
    
    # Crop the raster to the polygon boundary
    tst_crop <- terra::crop(tst_img, canopy_polygon)
    
    # Mask the cropped raster with the polygon
    tst_mask <- terra::mask(tst_crop, canopy_polygon)
    
    # Set the band names
    names(tst_mask) <- tst_names
    
    # Generate the output filename using the modified polygon name
    output_filename <- paste0("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/canopy_spectra/Canopy_Rasters/", polygon_name, ".ENVI")
    
    # Save the masked raster to a file
    writeRaster(tst_mask, output_filename, overwrite = TRUE)
    
    # Clean up memory after processing each polygon
    rm(tst_crop, tst_mask)
    gc()
  })
  
  # Clean up memory after processing the whole image
  rm(tst_img, tst_quads)
  gc()
})