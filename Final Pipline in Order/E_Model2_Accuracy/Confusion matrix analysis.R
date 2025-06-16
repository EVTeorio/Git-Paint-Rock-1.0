

Final_grouped_results <- readRDS("E:/Results/Final_Model.rds")
str(Final_grouped_results)

# Function to combine confusion matrices from Final_grouped_results[["VIs_only"]]
combine_confusion_matrices <- function(results) {
  # Extract iteration names (e.g., "Iter_1", "Iter_2", ...)
  iterations <- names(results)
  
  # Initialize combined confusion matrix as NULL
  combined_cm <- NULL
  
  for (iter in iterations) {
    # Extract confusion matrix from this iteration
    cm <- results[[iter]]$confusion_matrix$table
    
    # Convert to matrix if not already
    cm <- as.matrix(cm)
    
    # Initialize or add to combined confusion matrix
    if (is.null(combined_cm)) {
      combined_cm <- cm
    } else {
      # Add element-wise
      combined_cm <- combined_cm + cm
    }
  }
  
  return(combined_cm)
}

# Usage example:
# Assuming Final_grouped_results is loaded and has your data structure

combined_cm <- combine_confusion_matrices(Final_grouped_results[["VIs_only"]])
combined_cm<- combine_confusion_matrices(Final_grouped_results[["VIs_allLiDAR"]])

print(combined_cm)
#########################################################################

# --- Convert to data frame for plotting ---
cm_df <- as.data.frame(as.table(combined_cm)) %>%
  rename(Actual = Reference, Predicted = Prediction)

## --- Plot with Actual class on X-axis ---
ggplot(cm_df, aes(y = Predicted, x = Actual,  fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (VIs + All PAD metrics)",
    x = "Actual Class",
    y = "Predicted Class",
    fill = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
