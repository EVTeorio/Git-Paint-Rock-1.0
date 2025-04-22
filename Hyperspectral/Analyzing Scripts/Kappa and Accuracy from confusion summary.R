
# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)

# Initialize vectors to store accuracy and kappa values
accuracy_values <- c()
kappa_values <- c()

setwd("E:/Git Paint Rock 1.0/Output/ConfusionTables")

test <- read.csv("RawSpectra_Confusion_summary.csv")
str(test)
# List of CSV file names
csv_files <- c("RawSpectra_Confusion_summary.csv", "Sunlit_Confusion_summary.csv",
               "5nm_Sunlit_Confusion_summary.csv", "Sunlit_VegIndex_Confusion_summary.csv",
               "5nm_WithShadows_Confusion_summary.csv", "WithShadows_VegIndex_Confusion_summary.csv")

# Initialize vectors to store accuracy and kappa values
accuracy_values <- c()
kappa_values <- c()
script_names <- c()

# Loop through each CSV file, extract accuracy and kappa values
for (file in csv_files) {
  # Read the CSV file
  conf_matrix <- read.csv(file)
  
  # Extract the Accuracy and Kappa values from the second column (conf_matrix.overall)
  accuracy <- conf_matrix[conf_matrix$X == "Accuracy", "conf_matrix.overall"]
  kappa <- conf_matrix[conf_matrix$X == "Kappa", "conf_matrix.overall"]
  
  # Extract the part of the filename before '_Confusion_summary.csv' to use as the label
  script_name <- sub("_Confusion_summary.csv", "", file)
  
  # Store the extracted values and script names
  accuracy_values <- c(accuracy_values, accuracy)
  kappa_values <- c(kappa_values, kappa)
  script_names <- c(script_names, script_name)
}

# Create a data frame with the extracted values for plotting
results_df <- data.frame(
  Script = rep(script_names, each = 2),  # Repeat the script names for both Accuracy and Kappa
  Metric = rep(c("Accuracy", "Kappa"), times = length(script_names)),  # Metrics for each script
  Value = c(accuracy_values, kappa_values)  # Combine the accuracy and kappa values
)

# Plot the Accuracy and Kappa values side-by-side on the same graph
ggplot(results_df, aes(x = Script, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Accuracy and Kappa Values",
    x = "Script",
    y = "Values"
  ) +
  scale_fill_manual(values = c("skyblue", "orange")) +  # Custom colors for Accuracy and Kappa
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("E:/Git Paint Rock 1.0/Output/ConfusionTables/kappa_accuracy.png")
