

# Load required packages
library(lidR)
library(rgl)        # For visualization (optional)
library(ggplot2)    # For plotting
library(dplyr)      # Data manipulation

# Path to your .laz file
laz_file <- "E:/Updated LiDAR/PaintRock_20ha_leafOff_subset.laz"

# 1. Read the .laz file
laz <- readLAS(laz_file)

# 2. (option 1)Normalize the point cloud (remove terrain elevation)
dtm <- rasterize_terrain(laz, res = 1, algorithm = knnidw())
laz_norm <- normalize_height(laz, dtm)

# 3. Filter: remove ground and below-ground noise
laz_norm <- filter_poi(laz_norm, Classification != 2)  # Remove ground
laz_norm <- filter_poi(laz_norm, Z > 0)

# 4. Define custom branch complexity metric (vertical entropy)
branch_complexity_fun <- function(z, ...) {
  z <- z[is.finite(z) & z >= 0]
  if (length(z) < 10) return(as.numeric(NA))  # Force NA to be numeric
  bins <- seq(0, max(z) + 0.5, by = 0.5)
  h <- hist(z, breaks = bins, plot = FALSE)
  p <- h$counts / sum(h$counts)
  entropy <- -sum(p * log(p + 1e-10))
  return(entropy)
}


# 5. Compute raster grid of branch complexity (e.g., 10x10 m)
branch_raster <- grid_metrics(laz_norm, ~branch_complexity_fun(Z), res = 1)

# 6. Plot the raster
plot(branch_raster, main = "Branch Structure Complexity (Vertical Entropy)")
