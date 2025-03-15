
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

# Filter data to include rows with the response variable (SpeciesID) Change datset here
spec_chem_canopy_n25 <- spec_chem_canopy[!is.na(spec_chem_canopy[[className]]), ] %>%
  subset(TreeID != "not sampled") %>%
  mutate(SpeciesID = as.factor(SpeciesID),
         TreeID = as.factor(TreeID)) %>%
  group_by(TileNumber, TreeID, SpeciesID) %>%
  slice_sample(n = 80, replace = FALSE)

# Display counts for each group (e.g., Site, TreeID, SpeciesID)
spec_chem_canopy_n25 %>%
  group_by(TileNumber, TreeID, SpeciesID, eval(parse(text = className))) %>%
  tally() %>%
  print(n = 100)

# Check unique values of the response variable
unique(spec_chem_canopy_n25[[className]])

# Create a test and train split
inTrain <- caret::createDataPartition(
  y = spec_chem_canopy_n25[[className]],
  p = 0.7,
  list = FALSE
)

# Training and testing data subsets (only using TreeID and SpeciesID)
training <- spec_chem_canopy_n25[inTrain, ]

testing <- spec_chem_canopy_n25[-inTrain, ]

# Train the random forest model with probability predictions
rf_mod <- ranger::ranger(
  as.formula(paste(className, "~ .")),
  data = training, 
  num.trees = 1000, 
  probability = TRUE  # This enables prediction probabilities
)

# Predict on the testing data
rf_pred_prob <- predict(rf_mod, data = testing)

# Get the predicted probabilities (all classes) from the model
predicted_probabilities <- rf_pred_prob$predictions  # A matrix of probabilities

# Use `apply` and `which.max` to get the index of the class with the highest probability for each observation
predicted_class_index <- apply(predicted_probabilities, 1, which.max)

# Map these indices to the actual class labels (assuming columns of `predicted_probabilities` are the class labels)
predicted_class <- colnames(predicted_probabilities)[predicted_class_index]

# Extract the confidence (the highest probability for each prediction)
confidence_values <- apply(predicted_probabilities, 1, max)

# Combine predicted classes and their confidence values in a data frame for each sample
results <- data.frame(
  Predicted_Class = predicted_class,
  Confidence = confidence_values
)

# Calculate overall accuracy
actual_classes <- testing[[className]]
accuracy <- sum(predicted_class == actual_classes) / length(actual_classes)
cat("Overall Accuracy: ", accuracy, "\n")

# Create confusion matrix
conf_matrix <- caret::confusionMatrix(factor(predicted_class), factor(actual_classes))
print(conf_matrix)