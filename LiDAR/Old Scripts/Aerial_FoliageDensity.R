

# Load required libraries
library(lidR)
library(data.table)
library(terra)

# --- 1. Load LiDAR point cloud ---
las <- readLAS("E:/Updated LiDAR/PaintRock_20ha_leafOff_subset.laz")

# --- 2. Load trajectory file ---
trajectory <- fread("E:/Updated LiDAR/AMAPVox/2024-06-19_PaintRockFDP_20ha_traj.txt")

# --- 3. Rename columns in trajectory to standard names ---
setnames(trajectory, old = c("TIME", "X", "Y", "Z"),
         new = c("timestamp", "scannerX", "scannerY", "scannerZ"))

# --- 4. Prepare LAS points for join ---
las_points <- copy(las@data)
las_points[, gpstime := as.numeric(gpstime)]
trajectory[, timestamp := as.numeric(timestamp)]
setDT(las_points)
setDT(trajectory)
setkey(trajectory, timestamp)

# --- 5. Join trajectory (scanner positions) to each LAS point by nearest gpstime ---
joined <- trajectory[las_points, on = .(timestamp = gpstime), roll = "nearest"]

# --- 7. Compute ray vectors (scanner → return) ---
joined[, `:=`(
  vx = X - scannerX,
  vy = Y - scannerY,
  vz = Z - scannerZ
)]

# --- 6. Replace LAS data with enriched version ---
las@data <- as.data.frame(joined)

# --- 7. Voxel parameters ---
voxel_size <- 5.0  

# --- 8. Define voxel grid ---
min_x <- floor(min(las@data$X, na.rm = TRUE))
max_x <- ceiling(max(las@data$X, na.rm = TRUE))
min_y <- floor(min(las@data$Y, na.rm = TRUE))
max_y <- ceiling(max(las@data$Y, na.rm = TRUE))
min_z <- floor(min(las@data$Z, na.rm = TRUE))
max_z <- ceiling(max(las@data$Z, na.rm = TRUE))

dim_x <- ceiling((max_x - min_x) / voxel_size)
dim_y <- ceiling((max_y - min_y) / voxel_size)
dim_z <- ceiling((max_z - min_z) / voxel_size)

# --- 9. Initialize voxel hit and pass-through counters ---
voxel_hits <- array(0, dim = c(dim_x, dim_y, dim_z))
voxel_rays <- array(0, dim = c(dim_x, dim_y, dim_z))


trace_voxels <- function(x0, y0, z0, x1, y1, z1, size) {
  # Convert coordinates to voxel indices
  x0 <- floor((x0 - min_x) / size)
  y0 <- floor((y0 - min_y) / size)
  z0 <- floor((z0 - min_z) / size)
  x1 <- floor((x1 - min_x) / size)
  y1 <- floor((y1 - min_y) / size)
  z1 <- floor((z1 - min_z) / size)
  
  # Return NULL if any indices are NA or not finite
  if (any(is.na(c(x0, y0, z0, x1, y1, z1))) || any(!is.finite(c(x0, y0, z0, x1, y1, z1)))) {
    return(NULL)
  }
  
  # Check if voxel indices are within bounds
  if (x0 < 0 || y0 < 0 || z0 < 0 || x1 < 0 || y1 < 0 || z1 < 0 ||
      x0 >= dim_x || y0 >= dim_y || z0 >= dim_z || x1 >= dim_x || y1 >= dim_y || z1 >= dim_z) {
    return(NULL)  # Out of bounds voxel indices
  }
  
  # Line drawing using Bresenham's algorithm
  line <- list()
  dx <- abs(x1 - x0)
  dy <- abs(y1 - y0)
  dz <- abs(z1 - z0)
  sx <- ifelse(x0 < x1, 1, -1)
  sy <- ifelse(y0 < y1, 1, -1)
  sz <- ifelse(z0 < z1, 1, -1)
  err1 <- dx - dy
  err2 <- dx - dz
  
  x <- x0; y <- y0; z <- z0
  
  repeat {
    if (x >= 0 && y >= 0 && z >= 0 &&
        x < dim_x && y < dim_y && z < dim_z) {
      line[[length(line) + 1]] <- c(x + 1, y + 1, z + 1)
    }
    
    if (x == x1 && y == y1 && z == z1) break
    
    e2 <- 2 * err1
    e3 <- 2 * err2
    if (e2 > -dy) {
      err1 <- err1 - dy
      x <- x + sx
    }
    if (e2 < dx) {
      err1 <- err1 + dx
      y <- y + sy
    }
    if (e3 > -dz) {
      err2 <- err2 - dz
      x <- x + sx
    }
    if (e3 < dx) {
      err2 <- err2 + dz
      z <- z + sz
    }
  }
  
  do.call(rbind, line)
}


# --- 11. Collapse density vertically in height bands (15–30m in 10m steps) ---
band_heights <- seq(15, 25, by = 10)  # 15–25 and 25–35
density_slices <- list()

for (i in seq_along(band_heights)) {
  z_min <- floor((band_heights[i] - min_z) / voxel_size)
  z_max <- floor((band_heights[i] + 10 - min_z) / voxel_size)
  
  density_band <- matrix(0, nrow = dim_x, ncol = dim_y)
  for (x in 1:dim_x) {
    for (y in 1:dim_y) {
      density_band[x, y] <- sum(density[x, y, z_min:z_max], na.rm = TRUE)
    }
  }
  
  density_slices[[i]] <- density_band
}

# --- 12. Create raster stack ---
raster_stack <- rast()
for (i in seq_along(density_slices)) {
  r_band <- rast(nrows = dim_y, ncols = dim_x,
                 xmin = min_x, xmax = max_x,
                 ymin = min_y, ymax = max_y,
                 vals = t(density_slices[[i]]))
  names(r_band) <- paste0("density_", band_heights[i], "m")
  crs(r_band) <- crs(las@crs[["wkt"]])
  raster_stack <- c(raster_stack, r_band)
}

# --- 13. Plot the foliage density raster stack ---
plot(raster_stack, main = "Foliage Density by Height Band (1x1m)")
