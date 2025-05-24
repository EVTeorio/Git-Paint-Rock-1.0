
library(dplyr)
library(caret)
library(tidyr)
library(stringr)
library(beepr)
beep(3)


# Step 1–2: Your preprocessing steps remain the same
spec_chem_canopy <- read.csv("E:/Thesis_Final_Data/ALLmetrics_EndALL_BeALL.csv")
str(spec_chem_canopy)

canopies <- spec_chem_canopy %>%
  group_by(TreeID) %>%
  slice(1) %>%
  ungroup() %>%
  select(TreeID, SpeciesID)

# Filter species with more than 5 canopies
species_counts <- canopies %>%
  group_by(SpeciesID) %>%
  tally() %>%
  filter(n > 8)

canopies_filtered <- canopies %>%
  filter(SpeciesID %in% species_counts$SpeciesID)

# Step 3: Stratified split - 4 canopies per species for training
set.seed(42)
train_canopies <- canopies_filtered %>%
  group_by(SpeciesID) %>%
  slice_sample(n = 5) %>%
  ungroup()

# The rest go to testing
test_canopies <- anti_join(canopies_filtered, train_canopies, by = "TreeID")

# Step 4: Get full pixel data for each canopy
train_df <- spec_chem_canopy %>% filter(TreeID %in% train_canopies$TreeID)
test_df <- spec_chem_canopy %>% filter(TreeID %in% test_canopies$TreeID)

train_df$SpeciesID <- as.factor(train_df$SpeciesID)
test_df$SpeciesID <- as.factor(test_df$SpeciesID)

# Sample 50 pixels per species from the training set
set.seed(42)  # for reproducibility
balanced_train_df <- train_df %>%
  group_by(TreeID) %>%
  slice_sample(n = 500, replace = FALSE) %>%
  ungroup()
################################################################################
# Set the name of the class column
className <- "SpeciesID"

# Train the random forest model
rf_mod <- ranger::ranger(
  as.formula(paste(className, "~ .")),
  data = balanced_train_df, 
  num.trees = 100, 
  probability = TRUE
)

# Save the model to an RDS file
#saveRDS(rf_mod, file = "E:/Git Paint Rock 1.0/Hyperspectral/Models/rf_model_speciesID.rds")

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

