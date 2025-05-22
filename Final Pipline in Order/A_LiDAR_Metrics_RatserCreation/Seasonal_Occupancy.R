

# Load necessary library
library(terra)
forcomparison <- rast("E:/Updated LiDAR/PRFPD_CHM_leafOff.tiff")
plot(forcomparison)

# Load raster stacks
LeafOn <- rast("E:/LeafOn_Rasters/LeafOn_stack.tif")
LeafOff <- rast("E:/LiDAR_Metrics/LeafOff_Rasters/LeafOff_stack.tif")

# Subset only the layers from 20–35m
selected_layers <- c("PAD_20_25", "PAD_25_30","PAD_30_35")
LeafOn_sub <- LeafOn[[selected_layers]]
LeafOff_sub <- LeafOff[[selected_layers]]

# Compute the difference for selected layers
FoliageDiff_sub <- LeafOff_sub - LeafOn_sub
# Combine (sum) selected layers to get one total value for 20–35m
SeasonalOccupancy_20_35m <- sum(FoliageDiff_sub, na.rm=TRUE)
plot(SeasonalOccupancy_20_35m, main = "Seasonal Occupancy (20–35m)")

# Assign a clear name to the layer
names(SeasonalOccupancy_20_35m) <- "Seasonal_Occupancy_20_35m"

# Save and plot result
writeRaster(SeasonalOccupancy_20_35m, "E:/LiDAR_Metrics/Seasonal_Occupancy_20_35m.tif", overwrite=TRUE)

