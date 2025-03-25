

library(dplyr)
library(tidyr)
library(ranger)
library(caret)
library(ggplot2)
library(beepr)

# Read in data
spec_chem_canopy <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_masked.csv")
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

# Prepare for 100 iterations of bootstrapping
iterations <- 100
results <- data.frame(Iteration = integer(),
                      Accuracy = numeric(),
                      Sensitivity = numeric(),
                      Specificity = numeric())

# Loop to run the model 100 times with bootstrapped training data
for (i in 1:iterations) {
  
  # Create a random split for training and testing data each iteration
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
    data = training, # designating training sample
    num.trees = 1000, #Number of nodes used to make predictions
    probability = TRUE  # This enables prediction probabilities
  )
  
  # Predict on the testing data
  rf_pred_prob <- predict(rf_mod, data = testing)
  
  # Get the predicted probabilities (all classes) from the model
  predicted_probabilities <- rf_pred_prob$predictions  # A matrix of probabilities
  
  # Use `apply` and `which.max` to get the index of the class with the highest probability for each observation
  predicted_class_index <- apply(predicted_probabilities, 1, which.max)
  
  # Map these indices to the actual class labels
  predicted_class <- colnames(predicted_probabilities)[predicted_class_index]
  
  # Ensure the factor levels of predicted class and actual class are the same
  predicted_class <- factor(predicted_class, levels = levels(testing[[className]]))
  actual_class <- factor(testing[[className]], levels = levels(testing[[className]]))
  
  # Confusion Matrix
  conf_matrix <- caret::confusionMatrix(predicted_class, actual_class)
  
  # Extract performance metrics
  accuracy <- conf_matrix$overall['Accuracy']
  kappa <- conf_matrix$overall['Kappa']
  p_value <- conf_matrix$overall['AccuracyPValue']  # p-value for Accuracy compared to NIR
  
  # Store the results
  results <- rbind(results, data.frame(
    Iteration = i,
    Accuracy = accuracy,
    Kappa = kappa,
    P_Value = p_value
  ))
  
  beep()
}

print(results)
# Write the results to a CSV file
write.csv(results, "E:/Git Paint Rock 1.0/Output/RF_Model_Bootstrap_Results/bootstrap_model_results.csv",
          row.names = FALSE)

# Visualization
results_long <- results %>%
  gather(key = "Metric", value = "Value", Accuracy, Kappa)

# Plot the box and whisker plot
boxplot <- ggplot(results_long, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of Accuracy and Kappa",
    x = "Metric",
    y = "Value"
  ) +
  scale_fill_manual(values = c("skyblue", "lightgreen")) +
  theme(legend.position = "none")

# Save the boxplot to a file (for example, as a PNG)
ggsave("E:/Git Paint Rock 1.0/Output/RF_Model_Bootstrap_Results/boxplot_results.png",
       plot = boxplot, width = 8, height = 6, dpi = 300)

