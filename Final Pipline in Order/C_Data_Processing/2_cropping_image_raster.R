
library(terra)
library(raster)
library(sf)
library(beepr)
beep(3)

# Define file paths
HSI_dir <- "E:/Final_Rasters/Fusion_VI_Leafon_Images/"  # Directory containing rasters
canopies_path <- "E:/Git Paint Rock 1.0/Updated Canopy Polygons/Updated/"  # Path to shapefiles

# List all files in the hyperspectral image directory (without extensions)
allfiles <- list.files(HSI_dir)

# Exclude files that are not raster files
imgs <- allfiles[!grepl("\\.hdr$|\\.aux$|\\.xml$|\\.enp$|\\.sta$", allfiles)]  # Change this to match your file extensions

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

# Loop through matched files
lapply(matched_files, function(img_number) {
  cat("Processing image number:", img_number, "\n")
  
  img_idx <- which(img_numbers == img_number)
  img_path <- file.path(HSI_dir, imgs[img_idx])
  canopy_path <- canopies_sites[shapefile_numbers == img_number]
  
  if (!file.exists(img_path)) {
    warning(paste("Image file does not exist:", img_path))
    return(NULL)
  }
  
  # Load raster
  tst_img <- tryCatch({
    terra::rast(img_path)
  }, error = function(e) {
    warning(paste("Failed to load raster:", img_path, "-", e$message))
    return(NULL)
  })
  if (is.null(tst_img)) return(NULL)
  
  # Load shapefile
  tst_quads <- tryCatch({
    terra::vect(canopy_path)
  }, error = function(e) {
    warning(paste("Failed to load shapefile:", canopy_path, "-", e$message))
    return(NULL)
  })
  if (is.null(tst_quads)) return(NULL)
  
  # Validate geometry
  valid_geoms <- terra::is.valid(tst_quads)
  if (any(!valid_geoms)) {
    warning(paste("Invalid geometries found in shapefile:", canopy_path))
    tst_quads <- tst_quads[valid_geoms]
    if (length(tst_quads) == 0) {
      warning("No valid geometries remaining. Skipping.")
      return(NULL)
    }
  }
  
  # Check CRS match
  if (crs(tst_img) != crs(tst_quads)) {
    warning(paste("CRS mismatch between image and shapefile. Reprojecting shapefile:", canopy_path))
    tst_quads <- terra::project(tst_quads, crs(tst_img))
  }
  
  # Check extent overlap
  ext_overlap <- terra::intersect(terra::ext(tst_img), terra::ext(tst_quads))
  if (is.null(ext_overlap)) {
    warning(paste("Extent of shapefile does not overlap raster:", canopy_path))
    return(NULL)
  }
  
  # Process each polygon
  lapply(1:length(tst_quads), function(i) {
    canopy_polygon <- tst_quads[i]
    polygon_name <- canopy_polygon$Canopies
    if (is.null(polygon_name) | is.na(polygon_name)) {
      polygon_name <- paste0(i)
    }
    polygon_name <- paste0(img_number, "_", polygon_name)
    
    # Crop & mask
    tst_crop <- tryCatch({
      terra::crop(tst_img, canopy_polygon)
    }, error = function(e) {
      warning(paste("Failed to crop for polygon", polygon_name, "-", e$message))
      return(NULL)
    })
    if (is.null(tst_crop)) return(NULL)
    
    tst_mask <- tryCatch({
      terra::mask(tst_crop, canopy_polygon)
    }, error = function(e) {
      warning(paste("Failed to mask for polygon", polygon_name, "-", e$message))
      return(NULL)
    })
    if (is.null(tst_mask)) return(NULL)
    
    names(tst_mask) <- names(tst_img)
    
    output_filename <- paste0("E:/Final_Rasters/Canopies_VI_Leafon/", polygon_name, ".ENVI")
    
    tryCatch({
      writeRaster(tst_mask, output_filename, overwrite = TRUE)
    }, error = function(e) {
      warning(paste("Failed to write raster", output_filename, "-", e$message))
    })
    
    rm(tst_crop, tst_mask)
    gc()
  })
  
  rm(tst_img, tst_quads)
  gc()
})
beep()
