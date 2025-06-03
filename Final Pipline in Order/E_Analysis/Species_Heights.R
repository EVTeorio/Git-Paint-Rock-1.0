
library(terra)
library(sf)
library(dplyr)
library(ggplot2)

# Paths (replace with your actual paths)
canopies_path <- "E:/Git Paint Rock 1.0/Updated Canopy Polygons/Updated/"
liDAR_CHM_path <- "E:/Updated LiDAR/PRFPD_CHM_leafOn.tiff"

# Load CHM raster
chm_raster <- rast(liDAR_CHM_path)
chm_crs <- crs(chm_raster)

# List canopy shapefiles
shapefiles <- list.files(canopies_path, pattern = "\\.shp$", full.names = TRUE)

# Initialize list to store results
results_list <- list()

for (shp_path in shapefiles) {
  # Read canopy shapefile
  canopy_sf <- st_read(shp_path, quiet = TRUE)
  
  # Reproject to match CHM CRS
  canopy_sf <- st_transform(canopy_sf, crs = chm_crs)
  
  # Extract max height per polygon (using exact_extract for accuracy)
  # Note: exact_extract returns list with stats per polygon
  max_heights <- exactextractr::exact_extract(
    chm_raster, 
    canopy_sf, 
    'max', 
    progress = FALSE
  )
  
  # Combine with canopy IDs and SpeciesID (assuming columns exist)
  df <- canopy_sf %>%
    st_drop_geometry() %>%
    mutate(max_height = max_heights)
  
  results_list[[basename(shp_path)]] <- df
}
beep()
# Combine all results into one dataframe
all_canopies_df <- bind_rows(results_list)

# Check if SpeciesID column exists, otherwise parse it (example: from Canopies column)
all_canopies_df <- all_canopies_df %>%
  tidyr::separate(Canopies, into = c("TreeID", "SpeciesID"), sep = "_", remove = TRUE)


# Summarize number of canopies and max height statistics per species
species_summary <- all_canopies_df %>%
  group_by(SpeciesID) %>%
  summarise(
    num_canopies = n(),
    maximum_height = max(max_height, na.rm = TRUE),
    median_height = median(max_height, na.rm = TRUE),
    mean_height = mean(max_height, na.rm = TRUE),
    sd_height = sd(max_height, na.rm = TRUE)
  ) %>%
  arrange(desc(num_canopies))

print(n =33, species_summary)

write.csv(species_summary, "E:/Git Paint Rock 1.0/Output/LiDAR/Species_Height_Stats.csv")
##########################################################################################
# Create a label with species and number of canopies in parentheses
species_counts <- all_canopies_df %>%
  group_by(SpeciesID) %>%
  summarise(num_canopies = n()) %>%
  mutate(species_label = paste0(SpeciesID, " (", num_canopies, ")"))

# Join labels back to the full canopy data
all_canopies_df <- all_canopies_df %>%
  left_join(species_counts %>% select(SpeciesID, species_label), by = "SpeciesID")

# Plot boxplot with species label including counts
ggplot(all_canopies_df, aes(x = reorder(species_label, max_height, median), y = max_height)) +
  geom_boxplot(fill = "lightgreen", outlier.size = 1) +
  coord_flip() +
  labs(x = "Species (Number of Canopies)", y = "Max Canopy Height (m)",
       title = "Distribution of Max Canopy Heights per Species") +
  theme_minimal()
