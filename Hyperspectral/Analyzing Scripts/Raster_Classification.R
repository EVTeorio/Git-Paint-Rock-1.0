

# Load necessary libraries for raster processing
library(raster)
library(randomForest)
library(caret)
library(tidyverse)
library(beepr)
beep(11)

# Assuming the Random Forest model has already been trained and saved as 'rf_model'
# rf_model <- readRDS("rf_model.rds")

# 1. Load the raster file (assuming you have a raster of spectral bands)
# The raster file should have multiple bands corresponding to the spectral data you used
img_path <- "E:/Hyperspec Images/raw_11892_rd_rf_or"
img <- brick(img_path)

# The img object is a multi-band raster; we need to convert it into a matrix with rows as pixels and columns as bands
spectral_data <- as.data.frame((img))  # Extract values for each pixel

#resampling 5nm
df <- df_to_speclib(spectral_data, type="spectrolab")
df_resampled <- as.data.frame(spectrolab::resample(df, new_bands = seq(398, 999, 5), fwhm = 1))
df_resampled <- (df_resampled)

# Replace column "sample_name" with "X"
colnames(df_resampled)[colnames(df_resampled) == "sample_name"] <- "X"
rename_columns_with_X_except_first <- function(df_resampled) {
  # Modify column names by adding "X" except for the first column
  colnames(df_resampled)[-1] <- paste0("X", colnames(df_resampled)[-1])
  return(df_resampled)
}

For_Model <- rename_columns_with_X_except_first(df_resampled)

# Predict the species for each pixel using the pre-trained Random Forest model
predictions <- predict(rf_model, newdata = For_Model)

# Add the predicted species as a new column
spectral_data$predicted_species <- predictions

# Now, to map the predicted species back into a raster format:
# Create a data frame for the x, y coordinates and the predicted species
coords <- xyFromCell(img, 1:ncell(img))  # Extract x, y coordinates for each pixel

# Combine the coordinates with the predicted species
predicted_df <- data.frame(x = coords[, 1], y = coords[, 2], predicted_species = spectral_data$predicted_species)

# Convert the 'predicted_species' factor to numeric indices
predicted_df$predicted_species <- as.factor(predicted_df$predicted_species)

# Convert this back into a raster
predicted_raster <- rasterFromXYZ(predicted_df)

# Check the unique levels of predicted_species
species_levels <- levels(predicted_df$predicted_species)

# Create a color palette with a color for each species class
# Using a predefined color palette, e.g., "Set3" from RColorBrewer
library(RColorBrewer)
num_species <- length(species_levels)
colors <- brewer.pal(min(num_species, 12), "Set3")  # Use Set3 or any other palette

# Ensure that there are enough colors for all species levels
if (num_species > 12) {
  colors <- colorRampPalette(colors)(num_species)  # If there are more than 12 species, expand the palette
}

# Plot the raster using the custom color palette
plot(predicted_raster, main = "Predicted Species Map", col = colors, legend = TRUE)

# Plot the predicted species raster with categories (species labels)
plot(predicted_raster, main = "Predicted Species Map", col = rainbow(length(levels(predicted_df$predicted_species))))


# Optionally, save the predicted species raster to a file
writeRaster(predicted_raster, filename = "path_to_output_predicted_species.tif", format = "GTiff", overwrite = TRUE)

###################################################################################3
# Optionally, you can also create a visualization of the predictions by plotting them
# Here, we will use the levelplot function from the rasterVis package to create a better map visualization
# install.packages("rasterVis")  # Uncomment if not already installed
library(rasterVis)
levelplot(predicted_raster, main = "Predicted Species Map", col.regions = rainbow(5))

# The predicted_raster now contains the predicted class values for each pixel in the raster.
# You can further analyze, map, or export the results.
