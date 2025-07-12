
# Load necessary libraries for raster processing
library(raster)
library(tmap)
library(ggplot2)
library(lattice)
library(ranger)
library(caret)
library(tidyverse)
library(spectrolab)
library(rasterVis)
library(beepr)
beep()

# 1. Load the raster file (assuming you have a raster of spectral bands)
img_path <- "E:/Thesis_Final_Data/Fusion_VI_ALLmetrics_Images/raw_7995_rd_rf_or_VI_ALLmetrics.envi"
img <- brick(img_path)
plot(img)

# 2. Extract spectral values from the raster (assuming bands represent columns of spectral data)
# Convert raster object to a data frame
spectral_df <- as.data.frame(img, xy = TRUE)  # Convert to data frame, keeping XY coordinates


spectral_df <- justincase

beep()

#make sure metrics names align
# Your vector of VI names
vi_names <- c(
  "Boochs", "Boochs2", "CARI", "Carter", "Carter2", "Carter3", "Carter4", "Carter5", "Carter6",
  "CI", "CI2", "ClAInt", "CRI1", "CRI2", "CRI3", "CRI4", "D1", "D2", "Datt", "Datt2", "Datt3",
  "Datt4", "Datt5", "Datt6", "DD", "DDn", "DPI", "DWSI4", "EGFN", "EGFR", "EVI", "GDVI2",
  "GDVI3", "GDVI4", "GI", "Gitelson", "Gitelson2", "GMI1", "GMI2", "GreenNDVI", "Maccioni",
  "MCARI", "MCARIOSAVI", "MCARI2", "MCARI2OSAVI2", "mND705", "mNDVI", "MPRI", "MSAVI", "mSR",
  "mSR2", "mSR705", "MTCI", "MTVI", "NDVI", "NDVI2", "NDVI3", "NPCI", "OSAVI", "OSAVI2",
  "PARS", "PRI", "PRICI2", "PRInorm", "PSND", "PSRI", "PSSR", "RDVI", "REPLE", "REPLi",
  "SAVI", "SIPI", "SPVI", "SR", "SR1", "SR2", "SR3", "SR4", "SR5", "SR6", "SR7", "SR8", "SRPI",
  "SumDr1", "SumDr2", "TCARI", "TCARIOSAVI", "TCARI2", "TCARI2OSAVI2", "TGI", "TVI",
  "Vogelmann", "Vogelmann2", "Vogelmann3", "Vogelmann4"
)

# Get all column names
col_names <- colnames(spectral_df)

# Identify the columns to rename (those matching the VI pattern)
vi_cols <- grep("^raw_7995_rd_rf_or_VI_\\d+$", col_names)

# Sanity check: make sure there are 95 matches
if (length(vi_cols) != length(vi_names)) {
  stop("Mismatch between number of VI columns and VI names.")
}

# Replace the column names
colnames(spectral_df)[vi_cols] <- vi_names

# Define the variable groups
vi_vars <- c("mNDVI", "NPCI", "PSRI", "SR7")
leafon_vars <- c("PAD_20_25_on", "PAD_25_30_on", "PAD_30_35_on", "PAD_35_40_on")
leafoff_vars <- c("PAD_20_25_off", "PAD_25_30_off", "PAD_30_35_off", "PAD_35_40_off")
seasonal_var <- "Seasonal_Occupancy_20_35m"

# Combine all selected variables into one vector
selected_vars <- c(vi_vars, leafon_vars, leafoff_vars, seasonal_var)

# Filter the data frame to keep only those columns
spectral_df <- spectral_df[, selected_vars]

#Extracting RF Model
RDS <- readRDS("E:/Results/Final_Model.rds")
# Extract the model from Iteration 1
rf_mod <- RDS[["VIs_allLiDAR"]][["Iter_1"]]$model
print(rf_mod)

# Remove the 'x' and 'y' columns from the data (coordinates) since the model doesn't need them
spectral_df <- spectral_df[, -(1:2)]  # Removing the 'x' and 'y' coordinates

rf_pred_prob <- predict(rf_mod, data = spectral_df)

# Get the predicted probabilities (all classes) from the model
predicted_probabilities <- rf_pred_prob$predictions  # A matrix of probabilities

# Use `apply` and `which.max` to get the index of the class with the highest probability for each observation
predicted_class_index <- apply(predicted_probabilities, 1, which.max)

# Map these indices to the actual class labels (assuming columns of `predicted_probabilities` are the class labels)
predicted_class <- colnames(predicted_probabilities)[predicted_class_index]

# Extract the confidence (the highest probability for each prediction)
confidence_values <- apply(predicted_probabilities, 1, max)

# 9. Apply threshold to mask or assign "UNKNOWN" if confidence is below a set threshold
#confidence_threshold <- .3
#predicted_class[confidence_values < confidence_threshold] <- "UNKNOWN"

# Assign predictions and confidence
spectral_df$predictions <- as.factor(predicted_class)
spectral_df$confidence <- confidence_values

# Reload the spatial coordinates from the original image
coords <- as.data.frame(img, xy = TRUE)[, c("x", "y")]

# Convert factor predictions to numeric IDs
spectral_df$predictions_num <- as.numeric(spectral_df$predictions)  

# Combine with coordinates
results_df <- cbind(coords, spectral_df[, c("predictions_num", "confidence")])

# Create raster from numeric predictions
pred_raster <- rasterFromXYZ(results_df[, c("x", "y", "predictions_num")], crs = crs(img))

# Assign factor levels (species codes) as a data.frame inside a list
class_labels <- levels(spectral_df$predictions)
levels(pred_raster) <- list(data.frame(ID = 1:length(class_labels), SpeciesCode = class_labels))

# Optional: Also create a confidence raster
conf_raster <- rasterFromXYZ(results_df[, c("x", "y", "confidence")], crs = crs(img))

# === Plot with custom color legend === #
# Generate color palette
num_classes <- length(class_labels)
color_palette <- rainbow(num_classes)

# Plot the predicted class raster
plot(pred_raster, col = color_palette, legend = FALSE, main = "Predicted Species Codes")

# Add custom species code legend
legend("right",
       legend = class_labels,
       fill = color_palette,
       title = "Species Code",
       cex = 0.6,
       bty = "n",
       ncol = 2)  # Adjust for layout

# Plot the confidence raster separately
plot(conf_raster, main = "Prediction Confidence")

# Write to disk
writeRaster(pred_raster, "E:/Git Paint Rock 1.0/Output/Classified_Images/HSI_LiDAR_Classification.tif", format = "GTiff", overwrite = TRUE)
writeRaster(conf_raster, "E:/Git Paint Rock 1.0/Output/Classified_Images/HSI_LiDAR_Cofidence.tif", format = "GTiff", overwrite = TRUE)

