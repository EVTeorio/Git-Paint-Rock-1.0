

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

setwd("E:/Git Paint Rock 1.0/Output/ConfusionTables")
# Define a function to extract F1 scores for specific classes from CSV files
extract_f1_scores <- function(csv_file, classes) {
  # Load the CSV file
  class_metrics <- read.csv(csv_file, row.names = 1)  # Assuming class names are row names
  
  # Extract F1 scores for specified classes
  # The row names of the dataframe are the class names
  f1_scores <- class_metrics %>%
    rownames_to_column("Class") %>%  # Convert row names into a column called 'Class'
    filter(Class %in% classes) %>%   # Filter for the desired classes
    select(Class, F1)                # Select only the 'Class' and 'F1' columns
  
  return(f1_scores)
}

# List of CSV file names
csv_files <- c("RawSpectra_class_performance.csv", "Sunlit_class_performance.csv",
               "5nm_Sunlit_class_performance.csv", "Sunlit_VegIndex_class_performance.csv",
               "5nm_WithShadows_class_performance.csv", "WithShadows_VegIndex_class_performance.csv")

# Define the classes of interest
classes_of_interest <- c("Class: ACNE2", "Class: JUNI", "Class: PIEC2", "Class: QUAL", "Class: TIAM")

# Initialize an empty data frame to store the F1 scores
all_f1_scores <- data.frame()

# Loop through each CSV file and extract the F1 scores
for (csv_file in csv_files) {
  f1_scores <- extract_f1_scores(csv_file, classes_of_interest)
  f1_scores$File <- gsub("_class_performance.csv", "", basename(csv_file))  # Modify file name to remove _class_performance.csv
  
  # Clean the 'Class' column to remove 'Class:' prefix
  f1_scores$Class <- gsub("Class: ", "", f1_scores$Class)
  
  all_f1_scores <- bind_rows(all_f1_scores, f1_scores)
}

# Plot the F1 scores with cleaned class names
ggplot(all_f1_scores, aes(x = File, y = F1, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "F1 Scores for Selected Species",
       x = "", y = "F1 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")  # Use a color palette for better visuals

# Optionally, save the plot as a PNG or PDF file
ggsave("E:/Git Paint Rock 1.0/Output/ConfusionTables/f1_scores_plot.png")

