

# Load required packages
library(Rtsne)
library(ggplot2)
library(dplyr)
library(plotly)

# ---- Load Data ----
data <- read.csv("E:/Thesis_Final_Data/Balanced_Dataframes/Balanced_Sample_1.csv")

# ---- Preprocessing ----
#removing NAs
data_noNAs <- na.omit(data)

# Remove non-feature columns
non_feature_cols <- c("TileNumber", "SpeciesID", "TreeID")
feature_data <- data_noNAs %>% select(-all_of(non_feature_cols))

# Ensure all features are numeric 
feature_data <- feature_data %>% select(where(is.numeric))

# Scale the data
scaled_data <- scale(feature_data)

# ---- Run t-SNE ----
set.seed(42)  # for reproducibility
tsne_result <- Rtsne(scaled_data, dims = 3, perplexity = 30, verbose = TRUE, max_iter = 100)

# ---- Combine with Metadata for Plotting ----
tsne_df <- as.data.frame(tsne_result$Y)
tsne_df$SpeciesID <- data_noNAs$SpeciesID

# ---- Plot the Result ----
ggplot(tsne_df, aes(x = V2, y = V1, color = SpeciesID)) +
  geom_point(alpha = 0.4, size = 0.5) +
  labs(title = "t-SNE of Hyperspectral & LiDAR Features",
       x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

# Plot using plotly
plot_ly(
  data = tsne_df,
  x = ~X,
  y = ~Y,
  z = ~Z,
  color = ~SpeciesID,
  colors = "Set1",
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2)
) %>%
  layout(title = "3D t-SNE of Balanced Training Data")

