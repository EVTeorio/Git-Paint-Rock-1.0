
# Load necessary libraries for raster processing
library(raster)
library(ggplot2)
library(lattice)
library(ranger)
library(caret)
library(tidyverse)
library(spectrolab)
library(rasterVis)
library(beepr)
beep(3)

# 1. Load the raster file (assuming you have a raster of spectral bands)
img_path <- "E:/Hyperspec Images/raw_11892_rd_rf_or"
img <- brick(img_path)

# 2. Extract spectral values from the raster (assuming bands represent columns of spectral data)
# Convert raster object to a data frame
spectral_df <- as.data.frame(img, xy = TRUE)  # Convert to data frame, keeping XY coordinates
spectral_data <- spectral_df
spectral_df <- justincase
justincase <- spectral_df

# Remove the 'x' and 'y' columns from the data (coordinates) since the model doesn't need them
spectral_df <- spectral_df[, -(1:2)]  # Removing the 'x' and 'y' coordinates

# Now insert the first 4 columns of the repeated spec_chem_canopy_n25 after the second column of spectral_df
spectral_df <- cbind(spec_chem_canopy_n25_repeated[, 1:5], spectral_df[, 1:ncol(spectral_df)])


#RF Model required for the following
# Predict on the testing data
# Replicate the rows of spec_chem_canopy_n25 to match the number of rows in spectral_df
spec_chem_canopy_n25_repeated <- spec_chem_canopy_n25[rep(1:nrow(spec_chem_canopy_n25), length.out = nrow(spectral_df)), ]

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
confidence_threshold <- .3
predicted_class[confidence_values < confidence_threshold] <- "UNKNOWN"

spectral_df$predictions <- as.factor(predicted_class)
spectral_df$confidence <- confidence_values

# 10. Assuming spectral_data is your other dataframe with the first two columns you want to add
spectral_df <- cbind(spectral_data[, 1:2], spectral_df)

# Convert 'predictions' from factor to numeric values
spectral_df$predictions_numeric <- as.numeric(spectral_df$predictions)

# Now create the raster from the numeric predictions
predicted_raster <- rasterFromXYZ(spectral_df[, c("x", "y", "predictions_numeric")])
# save the raster of predictions
writeRaster(predicted_raster,
            filename = "E:/Git Paint Rock 1.0/Output/prediction_raster.tif",
            format = "GTiff", overwrite = TRUE)


# Define the color palette: One color for each species
category_colors <- rainbow(length(levels(spectral_df$predictions)))  # Or use other palettes, e.g., RColorBrewer::brewer.pal()

# Assign color labels to each category (species)
levels(predicted_raster) <- data.frame(ID = 1:length(levels(spectral_df$predictions)), 
                                       category = levels(spectral_df$predictions))

# Create the levelplot with labels for the raster
levelplot(predicted_raster, 
          main = "Predicted Species Map", 
          col.regions = category_colors,  # Colors for each species category
          at = 1:length(levels(spectral_df$predictions)),  # Values for each category (species)
          colorkey = list(labels = list(at = 1:length(levels(spectral_df$predictions)), 
                                        labels = levels(spectral_df$predictions))),  # Set labels for the legend
          scales = list(draw = TRUE))  # Optionally, add axis scales
##########################
# Optionally, create the raster for confidence values
confidence_raster <- rasterFromXYZ(spectral_df[, c("x", "y", "confidence")])
# Increase the margins to avoid the "figure margins too large" error
par(mar = c(5, 4, 4, 8))  # Increase the right margin to fit the legend
plot(confidence_raster,
     main = "Cofidence Distribution Map")

# Optionally, save the raster of predictions
writeRaster(confidence_raster,
            filename = "E:/Git Paint Rock 1.0/Output/Cofidence_Distribution_Ratser.tif", format = "GTiff", overwrite = TRUE)
