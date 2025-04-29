

library(sf)
library(dplyr)
library(stringr)
library(tools)
library(beepr)

# Input and output directories
input_dir <- "E:/Git Paint Rock 1.0/Hyperspectral/Updated Canopy Polygons/"
output_dir <- file.path(input_dir, "Updated")

# Create output folder if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get list of shapefiles
shapefiles <- list.files(input_dir, pattern = "\\.shp$", full.names = TRUE)

# Substitution list
substitutions <- list(
  "ACER" = "ACSA3C",
  "ACSA3" = "ACSA3C",
  "ACSA3CC" = "ACSA3C",
  "ACSAL" = "ACSA3C",
  "ACSAS2" = "ACSA3C",
  "AESCU" = "AEFL",
  "FRAM2" = "FRAMCO",
  "FRBI2" = "FRAMCO",
  "FRSM" = "FRAMCO",
  "FRAXI" = "FRAMCO",
  "CASHAG" = "CARYA",
  "CANUT" = "CARYA",
  "QUERC" = "QUMU",
  "QUMO4" = "QUMU",
  "ULOR" = "ULMUS",
  "CAOV3" = "CAOV2"
)

# Substitution function
substitute_species <- function(canopy_string) {
  parts <- str_split(canopy_string, "_", simplify = TRUE)
  if (ncol(parts) == 2) {
    prefix <- parts[1]
    species <- parts[2]
    for (pattern in names(substitutions)) {
      species <- sub(pattern, substitutions[[pattern]], species)
    }
    return(paste0(prefix, "_", species))
  } else {
    return(canopy_string)
  }
}

# Loop over shapefiles
for (shapefile in shapefiles) {
  shp <- st_read(shapefile, quiet = TRUE)
  
  if ("Canopies" %in% names(shp)) {
    # Safely modify Canopies field
    shp$Canopies <- vapply(shp$Canopies, substitute_species, character(1))
    
    # New layer name and output path
    original_name <- file_path_sans_ext(basename(shapefile))
    new_layer_name <- paste0(original_name)
    
    # Write shapefile
    st_write(shp,
             dsn = output_dir,
             layer = new_layer_name,
             driver = "ESRI Shapefile",
             delete_layer = TRUE,
             quiet = TRUE)
    
    cat("✅ Processed and saved:", file.path(output_dir, paste0(new_layer_name, ".shp")), "\n")
  } else {
    cat("⚠️  Skipping (no 'Canopies' field):", shapefile, "\n")
  }
}
beep()
