

library(raster)
library(sf)
library(lidR)
library(terra)
library(ggplot2)
library(dplyr)
library(beepr)
beep()

las <- readLAS("E:/Updated LiDAR/PaintRock_20ha_leafOn_Subset.laz")

help("delineate_crowns")

trees <- segment_trees(las, li2012(dt1 = 4, dt2 = 3, R = .5, Zu = 7, hmin = 15, speed_up = 5),
                       attribute = "treeID", uniqueness = "incremental")

crowns <- delineate_crowns(
  trees,
  type = c("convex", "concave", "bbox"),
  concavity = 1.5,
  length_threshold = 2,
  func = NULL
)

# Convert the sf object to a data frame for ggplot
crowns_df <- st_as_sf(crowns)
# Calculate centroids of each tree crown polygon
centroids <- st_centroid(crowns_df)

# Plot the tree crowns with centroids
ggplot() +
  geom_sf(data = crowns_df, fill = "grey", color = "black", alpha = 0.5) +  # Plot tree crowns
  geom_sf(data = centroids, aes(color = factor(treeID)), size = 3, shape = 3) +  # Plot centroids
  ggtitle("Tree Crowns and Centroids") +
  scale_color_identity() +  # Use the same color as the treeID
  theme_minimal() +
  theme(legend.position = "none")


# Plot with ggplot, using the row number as an identifier to color each polygon
ggplot(crowns_df) +
  geom_sf(aes(fill = factor(treeID)), color = "black", alpha = 0.5) +  # Use row_number() to assign colors
  ggtitle("Tree Crowns with Different Colors") +
  scale_fill_identity() +  # Use a color scale (e.g., viridis)
  theme_minimal() +
  theme(legend.position = "none")


# Export centroids to a shapefile
st_write(centroids, "E:/Git Paint Rock 1.0/Output/LiDAR Segmentation/centroids.shp", append = FALSE)

# Write the shapefile to disk
st_write(crowns_df, "E:/Git Paint Rock 1.0/Output/LiDAR/leafon_Segemention_43_57.shp", append = FALSE)
