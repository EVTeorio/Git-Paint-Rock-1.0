

# Load necessary packages
library(lidR)
library(rgl)

# Set path to your LAZ file
laz_file <- "E:/Updated LiDAR/PaintRock_20ha_leafOff_class.laz"

# Read the LAZ file
las <- readLAS(laz_file)


# ---- Filter Ground Points (Classification = 2) ----
ground_las <- filter_poi(las, Classification == 2)

# Check if there are enough ground points
if (is.empty(ground_las)) {
  stop("No ground points (Classification == 2) found in the LAS file.")
}

ground_elev <- ground_las@data$Z

# ---- Ground Elevation Metrics ----
min_elevation <- min(ground_elev, na.rm = TRUE)
max_elevation <- max(ground_elev, na.rm = TRUE)

# Average of 10 lowest and 10 highest ground elevations
sorted_ground <- sort(ground_elev, na.last = NA)
avg_lowest_10 <- mean(head(sorted_ground, 10))
avg_highest_10 <- mean(tail(sorted_ground, 10))

# ---- Normalize Heights: Tree Height Above Ground ----
las_norm <- normalize_height(las, algorithm = knnidw(k = 10, p = 2))
las_norm <- filter_poi(las_norm, Z >= 0)

max_tree_height <- max(las_norm@data$Z, na.rm = TRUE)

# ---- Output ----
cat("=== Forest Site Metrics (Based on Ground Classification) ===\n")
cat(sprintf("Minimum Ground Elevation: %.2f m\n", min_elevation))
cat(sprintf("Maximum Ground Elevation: %.2f m\n", max_elevation))
cat(sprintf("Average of 10 Lowest Ground Points: %.2f m\n", avg_lowest_10))
cat(sprintf("Average of 10 Highest Ground Points: %.2f m\n", avg_highest_10))
cat(sprintf("Maximum Tree Height (Above Ground): %.2f m\n", max_tree_height))