install.packages("RCSF")
library(RCSF)
library(beepr)
beep(11)
# libraries
library(lidR)
library(terra)

# Load and denoise LiDAR data.
las = readLAS("E:/KC Lidar files/PaintRock_20ha.laz")
las = classify_noise(las, ivf(res = 3, n = 10))
las = filter_poi(las, Classification != LASNOISE)
las = filter_poi(las, Z < 970)

# Classify ground points, create a dem and chm
las = classify_noise(las, ivf(res = 3, n = 10))
las = filter_poi(las, Classification != LASNOISE)
las = classify_ground(las, algorithm = csf()) #see `?csf` for more options
dem = rasterize_terrain(las, res=1, algorithm=tin())
csm = rasterize_canopy(las, algorithm = pitfree())
chm = csm - dem

# Convert the raster to a data frame
canopy_df <- as.data.frame(chm, xy = TRUE)

# Check if 'canopy_height' is numeric and convert if necessary
canopy_df$Z <- as.numeric(canopy_df$Z)

# Remove negative values from the canopy height data
canopy_df <- canopy_df[canopy_df$Z >= 0, ]

# Create the ggplot with both the raster and tree crowns
ggplot() +
  # Plot the canopy height raster as a background (with a gradient fill)
  geom_tile(data = canopy_df, aes(x = x, y = y, fill = Z), alpha = 0.5) +
  scale_fill_viridis_c() +  # Use a color scale for the canopy height
  # Plot the tree crowns
  geom_sf(data = crowns_df, aes(fill = factor(treeID)), color = "black", alpha = 0.7) +  # Overlay polygons
  ggtitle("Tree Crowns with Canopy Height Raster") +
  theme_minimal() +
  theme(legend.position = "none")

