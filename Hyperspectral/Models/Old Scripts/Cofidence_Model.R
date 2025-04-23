
library(dplyr)
library(tidyr)
library(ranger)
library(caret)
library(beepr)

# Read in data
spec_chem_canopy <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Outliers_Removed.csv")
colnames(spec_chem_canopy)

# Set seed for stable cal/val split
set.seed(1234)

# Set the response variable for modeling
className <- "SpeciesID"  # Adjust this variable as per the required response

# Filter data to include rows with the response variable (SpeciesID)
spec_chem_canopy_n25 <- spec_chem_canopy[!is.na(spec_chem_canopy[[className]]), ] %>%
  subset(TreeID != "not sampled") %>%
  mutate(SpeciesID = as.factor(SpeciesID),
         TreeID = as.factor(TreeID)) %>%
  group_by(TileNumber, TreeID, SpeciesID) %>%
  slice_sample(n = 80, replace = FALSE)

# Create a test and train split
inTrain <- caret::createDataPartition(
  y = spec_chem_canopy_n25[[className]],
  p = 0.7,
  list = FALSE
)

# Training and testing data subsets
training <- spec_chem_canopy_n25[inTrain, ]
testing <- spec_chem_canopy_n25[-inTrain, ]

# Train the random forest model with probability predictions
rf_mod <- ranger::ranger(
  as.formula(paste(className, "~ .")),
  data = training, 
  num.trees = 1000, 
  probability = TRUE  # This enables prediction probabilities
)

# Make predictions on the test set
response_predictions <- predict(rf_mod, data = testing, type = "response")

# Extract the predicted class probabilities
probabilities <- response_predictions$predictions

# For each prediction, check if the max class probability is above the threshold
max_probs <- apply(probabilities, 1, max)



#####################
# Set predictions to NA where the confidence is below the threshold
final_predictions <- ifelse(max_probs < threshold, NA, response_predictions$predictions)

# Remove NA predictions (where confidence was below threshold)
final_predictions_clean <- final_predictions[!is.na(final_predictions)]

# Get the true values from the test set (SpeciesID)
true_values <- testing$SpeciesID[!is.na(final_predictions)]

# Ensure that both final_predictions_clean and true_values have the same levels
# This is important to avoid the "more levels than the reference" issue
final_predictions_clean <- factor(final_predictions_clean, levels = levels(true_values))
true_values <- factor(true_values, levels = levels(true_values))

# Create the confusion matrix
conf_matrix <- confusionMatrix(final_predictions_clean, true_values)

# Print the confusion matrix
print(conf_matrix)



########################################

# Confidence Distribution Analysis:

# 1. Overall Confidence Distribution
# The overall confidence is simply the maximum probability for each observation
overall_confidence <- max_probs

# Display summary statistics of overall confidence
overall_confidence_summary <- summary(overall_confidence)
print("Overall Confidence Distribution Summary:")
print(overall_confidence_summary)

# 2. Confidence Distribution by SpeciesID
# We will group by SpeciesID and calculate the mean, standard deviation, and other statistics for confidence
confidence_by_species <- data.frame(SpeciesID = testing$SpeciesID, Confidence = max_probs)

species_confidence_stats <- confidence_by_species %>%
  group_by(SpeciesID) %>%
  summarise(
    Mean_Confidence = mean(Confidence, na.rm = TRUE),
    SD_Confidence = sd(Confidence, na.rm = TRUE),
    Min_Confidence = min(Confidence, na.rm = TRUE),
    Max_Confidence = max(Confidence, na.rm = TRUE),
    Median_Confidence = median(Confidence, na.rm = TRUE),
    Confidence_Range = Max_Confidence - Min_Confidence
  )

print("Confidence Statistics by SpeciesID:")
print(species_confidence_stats)

# Save the histogram of overall confidence using ggplot2
hist_plot <- ggplot(data.frame(Confidence = overall_confidence), aes(x = Confidence)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  labs(title = "Overall Confidence Distribution", x = "Confidence", y = "Frequency") +
  theme_minimal()

ggsave("E:/Git Paint Rock 1.0/Output/overall_confidence_histogram_ggplot.png", hist_plot, width = 8, height = 6)

# Save the boxplot of confidence by species using ggplot2
boxplot_plot <- ggplot(confidence_by_species, aes(x = SpeciesID, y = Confidence, fill = SpeciesID)) +
  geom_boxplot() +
  labs(title = "Confidence Distribution by SpeciesID", x = "SpeciesID", y = "Confidence") +
  theme_minimal()

ggsave("E:/Git Paint Rock 1.0/Output/confidence_by_species_boxplot_ggplot.png", boxplot_plot, width = 8, height = 6)


