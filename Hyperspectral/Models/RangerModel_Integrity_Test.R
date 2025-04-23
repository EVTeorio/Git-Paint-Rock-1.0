

library(dplyr)
library(tidyr)
library(ranger)
library(caret)
library(ggplot2)
library(beepr)

# Read in data
spec_chem_canopy <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_masked.csv")
colnames(spec_chem_canopy)

# Set seed for reproducibility
set.seed(1234)

# Set the response variable for modeling
className <- "SpeciesID"

# Prepare for 100 iterations of bootstrapping
iterations <- 100
results <- data.frame(Iteration = integer(),
                      Accuracy = numeric(),
                      Sensitivity = numeric(),
                      Specificity = numeric())

# Loop for bootstrapping iterations
for (i in 1:iterations) {
  
  # Step 1: Canopy-level dataframe
  canopies <- spec_chem_canopy %>%
    group_by(TreeID) %>%
    slice(1) %>%
    ungroup() %>%
    select(TreeID, SpeciesID)
  
  # Step 2: Filter out species with only one canopy
  species_counts <- canopies %>%
    group_by(SpeciesID) %>%
    tally() %>%
    filter(n > 1)
  
  canopies_filtered <- canopies %>%
    filter(SpeciesID %in% species_counts$SpeciesID)
  
  # Step 3: Stratified 50/50 split
  split_idx <- createDataPartition(canopies_filtered$SpeciesID, p = 0.5, list = FALSE)
  train_canopies <- canopies_filtered[split_idx, ]
  test_canopies <- canopies_filtered[-split_idx, ]
  
  # Step 4: Full pixel data for each canopy
  train_df <- spec_chem_canopy %>% filter(TreeID %in% train_canopies$TreeID)
  test_df <- spec_chem_canopy %>% filter(TreeID %in% test_canopies$TreeID)
  
  # Step 5: Subsample 50 pixels per species from training data
  train_df <- train_df %>%
    group_by(SpeciesID) %>%
    slice_sample(n = 50, replace = FALSE) %>%
    ungroup()
  
  # Ensure SpeciesID is a factor for model training
  train_df$SpeciesID <- as.factor(train_df$SpeciesID)
  test_df$SpeciesID <- as.factor(test_df$SpeciesID)
  
  # Step 6: Train the model
  rf_mod <- ranger::ranger(
    as.formula(paste(className, "~ .")),
    data = train_df,
    num.trees = 1000,
    probability = TRUE
  )
  
  # Step 7: Predict
  rf_pred_prob <- predict(rf_mod, data = test_df)
  predicted_probabilities <- rf_pred_prob$predictions
  predicted_class_index <- apply(predicted_probabilities, 1, which.max)
  predicted_class <- colnames(predicted_probabilities)[predicted_class_index]
  
  # Step 8: Evaluation
  predicted_class <- factor(predicted_class, levels = levels(test_df[[className]]))
  actual_class <- factor(test_df[[className]], levels = levels(test_df[[className]]))
  
  conf_matrix <- caret::confusionMatrix(predicted_class, actual_class)
  accuracy <- conf_matrix$overall['Accuracy']
  kappa <- conf_matrix$overall['Kappa']
  p_value <- conf_matrix$overall['AccuracyPValue']
  
  results <- rbind(results, data.frame(
    Iteration = i,
    Accuracy = accuracy,
    Kappa = kappa,
    P_Value = p_value
  ))
  
  beep()
}

# Print the results
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