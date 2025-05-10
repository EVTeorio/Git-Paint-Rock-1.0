install.packages("rgeos")

# Load required libraries
library(lidR)
library(sf)
library(dplyr)
library(caret)
library(rgeos)
library(purrr)
beep()

# Provide your file paths here
las_file <- "E:/Updated LiDAR/PaintRock_20ha_leafOn_Subset.laz"
ref_shp_file <- "E:/Git Paint Rock 1.0/Hyperspectral/Updated Canopy Polygons/25392_labels_QGIS.shp"
sf_use_s2(FALSE)

# Load LAS and reference crown shapefile
las <- readLAS(las_file)
ref_crowns <- st_read(ref_shp_file)

# Define evaluation area as the LAS extent
eval_area <- st_as_sfc(st_bbox(las), crs = st_crs(las))  # Correct CRS for LAS

# Clip LAS to evaluation extent
las <- clip_roi(las, eval_area)

# Now transform eval_area to match reference crown CRS for intersection
eval_area_for_crowns <- st_transform(eval_area, st_crs(ref_crowns))

# Clip reference crowns to the same area
ref_crowns <- st_intersection(ref_crowns, eval_area_for_crowns)

# Function to segment trees, delineate crowns, and compute IoU
evaluate_params <- function(dt1, dt2, R, Zu, hmin, speed_up, concavity) {
  tryCatch({
    trees <- segment_trees(
      las,
      li2012(dt1 = dt1, dt2 = dt2, R = R, Zu = Zu, hmin = hmin, speed_up = speed_up),
      attribute = "treeID", uniqueness = "incremental"
    )
    
    crowns <- delineate_crowns(
      trees,
      type = "concave", concavity = concavity,
      length_threshold = 2
    )
    
    pred_sf <- st_as_sf(crowns)
    
    matched_pairs <- st_intersects(pred_sf, ref_crowns)
    ious <- sapply(seq_along(matched_pairs), function(i) {
      if (length(matched_pairs[[i]]) == 0) return(0)
      pred_geom <- pred_sf[i, ]
      ref_geom <- ref_crowns[matched_pairs[[i]][1], ]
      inter <- st_area(st_intersection(pred_geom, ref_geom))
      union <- st_area(st_union(pred_geom, ref_geom))
      as.numeric(inter / union)
    })
    
    return(mean(ious, na.rm = TRUE))
  }, error = function(e) {
    return(NA)
  })
}

# Generate random hyperparameter grid
set.seed(123)
param_grid <- expand.grid(
  dt1 = sample(seq(2, 6, 1), 10, replace = TRUE),
  dt2 = sample(seq(1, 4, 1), 10, replace = TRUE),
  R = sample(seq(0.5, 2, 0.5), 10, replace = TRUE),
  Zu = sample(seq(5, 15, 2), 10, replace = TRUE),
  hmin = sample(seq(10, 20, 2), 10, replace = TRUE),
  speed_up = sample(seq(1, 10, 2), 10, replace = TRUE),
  concavity = sample(seq(1, 3, 0.5), 10, replace = TRUE)
) %>% distinct()

# Run evaluation
results <- param_grid %>%
  mutate(score = pmap_dbl(., ~ evaluate_params(..1, ..2, ..3, ..4, ..5, ..6, ..7)))

# Best parameters
best_params <- results %>% arrange(desc(score)) %>% slice(1)
print("Best Parameters Found:")
print(best_params)
