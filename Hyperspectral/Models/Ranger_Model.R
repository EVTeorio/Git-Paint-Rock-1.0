
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

# Ranger models (Random Forest)
n <- 1000  # Number of trees
rf_mod <- ranger::ranger(as.formula(paste(className, "~ .")),
                         data = training, num.trees = n)
###################################
# Make predictions on the test data
rf_mod_pred <- predict(rf_mod, testing)

# Extract predicted class labels
predicted_classes <- rf_mod_pred$predictions

# Calculate overall accuracy
actual_classes <- testing[[className]]
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)
cat("Overall Accuracy: ", accuracy, "\n")

# Create confusion matrix
conf_matrix <- confusionMatrix(factor(predicted_classes), factor(actual_classes))
print(conf_matrix)
