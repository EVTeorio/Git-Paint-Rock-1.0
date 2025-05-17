# compare lidar CHMs with plot data
# March 5, 2025
# KC Cushman

#### setup and read data ####
library(terra)
library(allodb)
setwd("E:/Updated LiDAR")
plotData <- read.csv("E:/BASE_PR_20/Updated Census/Crown345_JAN2025.csv")

chm <- rast("PRFPD_CHM_leafOn.tiff")

plotLocs <- vect("20ha.kml")
plotLocs_UTM <- project(plotLocs,crs(chm))

#### aggregate plot data by quadrat ####

plotSummary <- aggregate(plotData$agb, by=list(plotData$quadrat), FUN=sum)
names(plotSummary) <- c("quadrat","agbSum")

# convert agb to Mg/ha
# first confirm data is currently in kg
test <- allodb::get_biomass(dbh = head(plotData$dbh/10), 
                            genus = head(plotData$Genus), 
                            species = head(plotData$Species), 
                            coords = c(head(plotData$long,1), head(plotData$lat,1)))
test
head(plotData$agb)

plotSummary$agb_Mgha <- plotSummary$agbSum*(1/1000)*(10000/20^2)

#### summarize chm per quadrat ####

# make columns to store chm summary metrics
plotSummary$lidar_sd <- NA
plotSummary$lidar_cv <- NA
plotSummary$lidar_mch <- NA
plotSummary$lidar_pm <- NA
plotSummary$lidar_p2m <- NA
plotSummary$lidar_p50 <- NA
plotSummary$lidar_p75 <- NA
plotSummary$lidar_p95 <- NA
plotSummary$lidar_rumple <- NA

for(i in 1:nrow(plotSummary)){
  # crop chm to quadrat of interest
  chm_i <- crop(chm, plotLocs_UTM[plotLocs_UTM$Name==plotSummary$quadrat[i],])
  # mask chm to quadrat of interest
  chm_i <- mask(chm_i, plotLocs_UTM[plotLocs_UTM$Name==plotSummary$quadrat[i],])
  # extract chm values
  chm_values <- values(chm_i)
  # remote NA values because of tilted plots
  chm_values <- chm_values[!is.na(chm_values)]
  
  plotSummary$lidar_sd[i] <- sd(chm_values) # Calculated LiDAR metric - Standard deviation of heights
  plotSummary$lidar_cv[i] <- sd(chm_values)/mean(chm_values) # Calculated LiDAR metric - Coefficient of variation of heights
  plotSummary$lidar_mch[i] <- mean(chm_values) # Calculated LiDAR metric - Mean canopy height
  plotSummary$lidar_pm[i] <- 100*length(chm_values[chm_values>mean(chm_values)])/length(chm_values) # Calculated LiDAR metric - Percentage of points above mean height
  plotSummary$lidar_p2m[i] <- 100*length(chm_values[chm_values>2])/length(chm_values) # Calculated LiDAR metric - Percentage of points over 2 meters
  plotSummary$lidar_p50[i] <- quantile(chm_values, 0.5) # Calculated LiDAR metric - 50th percentile of height
  plotSummary$lidar_p75[i] <- quantile(chm_values, 0.75) # Calculated LiDAR metric - 75th percentile of height
  plotSummary$lidar_p95[i] <- quantile(chm_values, 0.95) # Calculated LiDAR metric - 95th percentile of height
  plotSummary$lidar_rumple[i] <- lidR::rumple_index(chm_i)
}

#### compare plot and lidar metrics ####

plot(agb_Mgha ~ lidar_mch, data = plotSummary,
     pch=19, col=adjustcolor("black",0.5))
