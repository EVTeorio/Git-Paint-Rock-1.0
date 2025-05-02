
library(dplyr)
library(tidyr)
library(ranger)
library(caret)
library(beepr)
beep()

# Read in data
spec_chem_canopy <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/5nm_Sunlit.csv")
colnames(spec_chem_canopy)
spec_chem_canopy <- mean_vegetation_indices
###########QA/QC
# Filter rows 
#spec_chem_canopy <- spectral_df
#unique(Subset_Data$TileNumber)

#Subset_Data <- spec_chem_canopy

#spec_chem_canopy <- Subset_Data[Subset_Data$TileNumber == 32619, ]
######################



train_df$SpeciesID <- as.factor(train_df$SpeciesID)
test_df$SpeciesID <- as.factor(test_df$SpeciesID)

# Sample 50 pixels per species from the training set
set.seed(42)  # for reproducibility
balanced_train_df <- train_df %>%
  group_by(SpeciesID) %>%
  slice_sample(n = 50, replace = FALSE) %>%
  ungroup()

# Set the name of the class column
className <- "SpeciesID"

# Train the random forest model using canopy-level split
rf_mod <- ranger::ranger(
  as.formula(paste(className, "~ .")),
  data = balanced_train_df, 
  num.trees = 1000, 
  probability = TRUE
)

# Save the model to an RDS file
saveRDS(rf_mod, file = "E:/Git Paint Rock 1.0/Hyperspectral/Models/rf_model_speciesID.rds")

# Predict on the testing data
rf_pred_prob <- predict(rf_mod, data = test_df)

# Get predicted probabilities
predicted_probabilities <- rf_pred_prob$predictions

# Predicted class (max probability per row)
predicted_class_index <- apply(predicted_probabilities, 1, which.max)
predicted_class <- colnames(predicted_probabilities)[predicted_class_index]

# Confidence values (max probability)
confidence_values <- apply(predicted_probabilities, 1, max)

# Combine predicted classes and confidence
results <- data.frame(
  Predicted_Class = predicted_class,
  Confidence = confidence_values
)

# Actual classes from the test data
actual_classes <- test_df[[className]]

# Add actual class to results for evaluation
results$Actual_Class <- actual_classes

# Calculate accuracy
accuracy <- sum(results$Predicted_Class == results$Actual_Class) / nrow(results)
cat("Overall Accuracy: ", accuracy, "\n")

# Create confusion matrix
conf_matrix <- caret::confusionMatrix(
  factor(results$Predicted_Class, levels = unique(actual_classes)),
  factor(results$Actual_Class, levels = unique(actual_classes))
)
print(conf_matrix)
