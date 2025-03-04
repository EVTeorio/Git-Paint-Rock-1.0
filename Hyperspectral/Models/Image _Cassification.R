

# Load necessary libraries for raster processing
library(raster)
library(ggplot2)
library(ranger)
library(caret)
library(tidyverse)
library(spectrolab)
library(rasterVis)
library(beepr)
beep(3)

# Assuming the Random Forest model has already been trained and saved as 'rf_model'
# rf_model <- readRDS("rf_model.rds") # Uncomment if you have a pre-trained model

# 1. Load the raster file (assuming you have a raster of spectral bands)
img_path <- "E:/Hyperspec Images/raw_11892_rd_rf_or"
img <- brick(img_path)

# 2. Extract spectral values from the raster (assuming bands represent columns of spectral data)
# Convert raster object to a data frame
spectral_df <- as.data.frame(img, xy = TRUE)  # Convert to data frame, keeping XY coordinates
spectral_data <- spectral_df
spectral_df <- spectral_data

# Replicate the rows of spec_chem_canopy_n25 to match the number of rows in spectral_df
spec_chem_canopy_n25_repeated <- spec_chem_canopy_n25[rep(1:nrow(spec_chem_canopy_n25), length.out = nrow(spectral_df)), ]

# 4. Remove the 'x' and 'y' columns from the data (coordinates) since the model doesn't need them
spectral_df <- spectral_data[, -(1:2)]  # Removing the 'x' and 'y' coordinates

# Now insert the first 4 columns of the repeated spec_chem_canopy_n25 after the second column of spectral_df
spectral_df <- cbind(spec_chem_canopy_n25_repeated[, 1:5], spectral_df[, 1:ncol(spectral_df)])


# 5. Predict using the trained Random Forest model
# Assuming the model is already trained and saved as 'rf_mod'
rf_mod_pred <- predict(rf_mod, spectral_df)

# 6. Store predictions into the data frame (this will be the predicted class for each pixel)
spectral_df$predictions <- rf_mod_pred$predictions

# Assuming spectral_data is your other dataframe with the first two columns you want to add
spectral_df <- cbind(spectral_data[, 1:2], spectral_df)

# Convert species labels to numeric values
spectral_df$numeric_predictions <- as.numeric(factor(spectral_df$predictions))

# Create the raster from the numeric predictions
predicted_raster <- rasterFromXYZ(spectral_df[, c("x", "y", "numeric_predictions")])

# Check the unique levels of predicted_species
species_levels <- levels(spectral_df$numeric_predictions)

# Generate a custom legend with species labels
legend_labels <- levels(spectral_df$predictions)

# Create the plot and add the legend
levelplot(predicted_raster, 
          main = "Predicted Species Map", 
          col.regions = rainbow(length(legend_labels)),
          at = seq(1, length(legend_labels), by = 1), 
          colorkey = list(labels = list(at = seq(1, length(legend_labels), by = 1), 
                                        labels = legend_labels)))

# Increase the margins to avoid the "figure margins too large" error
par(mar = c(5, 4, 4, 8))  # Increase the right margin to fit the legend

# Plot the raster with a discrete color palette and the legend
plot(predicted_raster, 
     main = "Predicted Species Map", 
     col = rainbow(length(species_levels)),  # Discrete color palette based on number of species
     legend = TRUE,  # Show legend
     axes = TRUE,  # Show axes
     legend.args = list(text = "Species", side = 4, line = 3, cex = 0.8))  # Add legend title for clarity

# Optionally, save the raster of predictions
writeRaster(predicted_raster, filename = "predicted_class_raster.tif", format = "GTiff", overwrite = TRUE)

