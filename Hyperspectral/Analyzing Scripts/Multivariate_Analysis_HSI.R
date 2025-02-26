
install.packages("car")
install.packages("quantreg")

# Load necessary libraries
# Load the plotly package for 3D visualization
library(plotly)
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(car)

# Read in the CSV file
df <- read.csv(
  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_masked_5nm.csv")

# Select only the numeric columns for spectral bands and keep SpeciesID for classification
spectral_data <- df %>%
  select(-c(TileNumber, TreeID, SpeciesID))  # Remove non-numeric columns (like TileNumber, TreeID)

species_data <- df$SpeciesID  # Target variable (SpeciesID)

# Scaling the data to ensure all spectral bands are on the same scale
spectral_data_scaled <- scale(spectral_data)

# 1. Principal Component Analysis (PCA)
pca_result <- prcomp(spectral_data_scaled, center = TRUE, scale. = TRUE)
pca_variance <- summary(pca_result)$importance[2,]  # Proportion of variance explained by each PC

# Visualizing the PCA results
# Scree plot to show the proportion of variance explained by each principal component
scree_plot <- ggplot(data.frame(PC = 1:length(pca_variance), Variance = pca_variance), aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Scree Plot - PCA", x = "Principal Component", y = "Proportion of Variance") +
  theme_minimal()

# Display the scree plot
print(scree_plot)

# Biplot to visualize the first two principal components
biplot_pca <- fviz_pca_biplot(pca_result, geom = c("point"), 
                              col.ind = species_data, # Color points by SpeciesID
                              palette = "Set1", addEllipses = TRUE, legend.title = "SpeciesID")

# Display PCA biplot
print(biplot_pca)

# 2. Correlation Matrix (for identifying bands that are highly correlated)
correlation_matrix <- cor(spectral_data_scaled)
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# 3. Random Forest to identify important bands
set.seed(42)  # Set a seed for reproducibility

# Create a training set and a test set (70% training, 30% test)
trainIndex <- createDataPartition(species_data, p = 0.7, list = FALSE)
train_data <- spectral_data[trainIndex, ]
train_species <- species_data[trainIndex]
test_data <- spectral_data[-trainIndex, ]
test_species <- species_data[-trainIndex]

# Train the Random Forest model
rf_model <- randomForest(x = train_data, y = as.factor(train_species), importance = TRUE, ntree = 500)

# Convert the importance matrix into a data frame
importance_df <- as.data.frame(importance(rf_model))
importance_df$Band <- rownames(importance_df)  # Add the spectral bands as a new column

# Plot the importance of the bands
importance_plot <- ggplot(importance_df, aes(x = reorder(Band, -MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Random Forest Variable Importance", x = "Spectral Band", y = "Mean Decrease in Gini Index") +
  theme_minimal() +
  coord_flip()

# Display the importance plot
print(importance_plot)

# 4. Model performance: Predict the test set and calculate accuracy
rf_predictions <- predict(rf_model, newdata = test_data)
rf_accuracy <- sum(rf_predictions == test_species) / length(test_species)
cat("Random Forest Model Accuracy: ", rf_accuracy * 100, "%\n")

# 5. Visualizing the confusion matrix
confusion_matrix <- confusionMatrix(factor(rf_predictions), factor(test_species))
print(confusion_matrix)

# 6. Optional: Cluster analysis based on SpeciesID
# Add SpeciesID as the Cluster label (supervised clustering)
df$Cluster <- as.factor(species_data)

# Extract the first three principal components from PCA results
pc1 <- pca_result$x[, 1]  # First principal component
pc2 <- pca_result$x[, 2]  # Second principal component
pc3 <- pca_result$x[, 3]  # Third principal component

# Create a 3D plot using plotly
pca_3d_plot <- plot_ly(df, x = ~pc1, y = ~pc2, z = ~pc3, color = ~Cluster, 
                       colors = "Set1", type = 'scatter3d', mode = 'markers',
                       marker = list(size = 2)) %>%
  layout(title = "3D PCA with SpeciesID Clusters",
         scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         legend = list(
           tracegroupgap = 10,  # Spacing between legend groups
           itemclick = "toggleothers",  # Allows toggling the visibility of groups
           itemsizing = "constant",  # Make the item sizes uniform
           font = list(size = 12),  # Adjust font size for legend labels
           marker = list(
             size = 8  # Adjust the size of the legend dots
           )
         ))

# Display the 3D PCA plot
pca_3d_plot
