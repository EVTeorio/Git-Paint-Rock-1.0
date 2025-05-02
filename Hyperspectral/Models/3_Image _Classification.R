
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
img_path <- "E:/Hyperspec Images/raw_7995_rd_rf_or"
img <- brick(img_path)

# 2. Extract spectral values from the raster (assuming bands represent columns of spectral data)
# Convert raster object to a data frame
spectral_df <- as.data.frame(img, xy = TRUE)  # Convert to data frame, keeping XY coordinates
#spectral_data <- spectral_df
#spectral_data <- justincase
# justincase <- spectral_df

###Calculate vegetation indices for the pixels(for VegIndex Models)#####
#tree_image_spectra <- spectral_df
#trees_image_spectra_df <- speclib_to_df(tree_image_spectra)
#trees_image_spectra_VIs <- get_vegetation_indices(trees_image_spectra_df, NULL)
beep()

# Remove the 'x' and 'y' columns from the data (coordinates) since the model doesn't need them
spectral_df <- spectral_df[, -(1:2)]  # Removing the 'x' and 'y' coordinates

#RF Model required for the following
# Predict on the testing data
# Replicate the rows of spec_chem_canopy_n25 to match the number of rows in spectral_df
spec_chem_canopy_n25_repeated <- spec_chem_canopy[rep(1:nrow(spec_chem_canopy), length.out = nrow(spectral_df)), ]

# Now insert the first 4 columns of the repeated spec_chem_canopy_n25 after the second column of spectral_df
spectral_df <- cbind(spec_chem_canopy_n25_repeated[, 1:4], spectral_df[, 1:ncol(spectral_df)])

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
spectral_df$predictions_num <- as.numeric(spectral_df$predictions)  # 1, 2, 3...

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
writeRaster(pred_raster, "E:/Git Paint Rock 1.0/Output/Classified_Images/7995_classified_sunlit.tif", format = "GTiff", overwrite = TRUE)
writeRaster(conf_raster, "E:/Git Paint Rock 1.0/Output/Classified_Images/7995_confidence_sunlit.tif", format = "GTiff", overwrite = TRUE)

