beep(3)
spec_chem_canopy <- MD_spectra


# Set seed for stable cal/val split
set.seed(1234)

# Set the response variable for modeling
className <- "SpeciesID"  # Adjust this variable as per the required response

# Filter data to include rows with the response variable (SpeciesID), and remove rows where TreeID is "not sampled"
spec_chem_canopy_n25 <- spec_chem_canopy[!is.na(spec_chem_canopy[[className]]), ] %>%
  subset(TreeID != "not sampled") %>%
  mutate(SpeciesID = as.factor(SpeciesID),
         TreeID = as.factor(TreeID)) %>%
  group_by(TileNumber, TreeID, SpeciesID) %>%
  slice_sample(n = 50, replace = FALSE)

# Remove SpeciesID with only one unique TreeID before the Cal/Val split
spec_chem_canopy_n25_filtered <- spec_chem_canopy_n25 %>%
  group_by(SpeciesID) %>%
  filter(n_distinct(TreeID) > 1) %>%
  ungroup()

# Check unique values of the response variable after filtering
unique(spec_chem_canopy_n25_filtered[[className]])

# Create the stratified split using caret's createDataPartition
# This will split the data while maintaining the SpeciesID and TreeID groupings
inTrain <- caret::createDataPartition(
  y = spec_chem_canopy_n25_filtered$SpeciesID, 
  p = 0.5, 
  list = FALSE
)

# Split the data into training and testing sets
training <- spec_chem_canopy_n25_filtered[inTrain, ]
testing <- spec_chem_canopy_n25_filtered[-inTrain, ]

# Check the number of TreeIDs included in the training and testing sets per SpeciesID
training_treeids_count <- training %>%
  group_by(SpeciesID) %>%
  summarise(Num_TreeIDs_Train = n_distinct(TreeID))

testing_treeids_count <- testing %>%
  group_by(SpeciesID) %>%
  summarise(Num_TreeIDs_Test = n_distinct(TreeID))

# Merge the counts to show both training and testing counts
treeids_count_summary <- left_join(training_treeids_count, testing_treeids_count, by = "SpeciesID")

# Display the summary of TreeID counts per SpeciesID
print(treeids_count_summary)

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


