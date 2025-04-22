

library(spectrolab)
library(torch)
library(RStoolbox)

setwd("C:/lecospec")
source("C:/lecospec/Functions/lecospectR.R")

#Read in plot image spectra
tree_image_spectra <- df_resampled
trees_image_spectra<-
  read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/5nm_Sunlit.csv")
trees_image_spectra_df <- speclib_to_df(tree_image_spectra)

#Calculate vegetation indices for the pixels
trees_image_spectra_VIs <- get_vegetation_indices(trees_image_spectra_df, NULL)
beep()

tree_image_spectra_VIs_bind <- cbind(as.data.frame(tree_image_spectra)[,1:3],trees_image_spectra_VIs) 
write.csv(tree_image_spectra_VIs_bind,  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Sunlit_VegIndex_5nm.csv")
