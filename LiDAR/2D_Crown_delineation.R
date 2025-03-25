

library(raster)
library(sf)
library(lidR)
library(terra)
library(ggplot2)

# Read the raster file
chm <- raster("E:/Updated LiDAR/PRFPD_CHM_leafOn.tiff")

# Define the extent for cropping (xmin, xmax, ymin, ymax)
crop_extent <- extent(563200, 563600, 3847800, 3848100)
# Crop the raster using the defined extent
chm <- crop(chm, crop_extent)

treetops = locate_trees(chm, lmf(ws = 7, hmin=18))
plot(crowns)
plot(treetops$geometry, add = TRUE, pch = 16, cex = 0.2)


crown_delineation_algorithm = dalponte2016(chm, treetops, th_tree = 5, th_cr = .99, th_seed = .5, max_cr = 10) 
                                           
crown_raster = crown_delineation_algorithm()
proj4string(crown_raster) <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")


# Assuming crown_raster is your RasterLayer
crowns <- rasterToPolygons(crown_raster, na.rm = TRUE)
# Convert SpatialPolygonsDataFrame to sf object
crowns_sf <- st_as_sf(crowns)



plot(crowns_sf, col=palette.colors(8000))
plot(chm); plot(crowns, border=grey(0.5), add = TRUE)

# Assuming your polygons have a factor column (e.g., 'category') for discrete colors
ggplot(crowns_sf) +
  geom_sf(aes(fill = category)) +  # Replace 'category' with your actual column name
  scale_fill_brewer(palette = "Set3") +  # You can use different color palettes (e.g., 'Set3')
  theme_minimal() +
  ggtitle("Crown Delineation")
# convert treetops to vect object and write to disk
writeVector(vect(treetops), 'treetops.shp')
writeVector(crowns, 'crowns.shp')