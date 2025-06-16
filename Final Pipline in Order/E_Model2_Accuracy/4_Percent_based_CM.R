

library(ggplot2)
library(dplyr)

# Load results
Final_grouped_results <- readRDS("E:/Results/Final_Model.rds")
str(Final_grouped_results)

# --- Function to combine confusion matrices from multiple iterations ---
combine_confusion_matrices <- function(results) {
  iterations <- names(results)
  combined_cm <- NULL
  
  for (iter in iterations) {
    cm <- results[[iter]]$confusion_matrix$table
    cm <- as.matrix(cm)
    
    if (is.null(combined_cm)) {
      combined_cm <- cm
    } else {
      combined_cm <- combined_cm + cm
    }
  }
  
  return(combined_cm)
}

# Combine confusion matrices for the desired model
combined_cm <- combine_confusion_matrices(Final_grouped_results[["VIs_allLiDAR"]])

# --- Compute row totals and prepare labels for y-axis (Actual) ---
row_totals <- colSums(combined_cm)
actual_labels <- paste0(rownames(combined_cm), " (n = ", row_totals, ")")

# --- Convert to column-wise percentages ---
cm_percent <- prop.table(combined_cm, margin = 2) * 100  # Column-wise percentages
cm_df <- as.data.frame(as.table(cm_percent)) %>%
  rename(Actual = Reference, Predicted = Prediction)

# --- Format percentage labels ---
cm_df$Label <- sprintf("%.1f%%", cm_df$Freq)

# --- Ensure Predicted is in correct order for x-axis (unchanged) ---
cm_df$Predicted <- factor(cm_df$Predicted, levels = colnames(combined_cm))

# --- Replace Actual with labeled factor using row totals ---
label_map <- setNames(actual_labels, rownames(combined_cm))
cm_df$Actual <- factor(cm_df$Actual, levels = rownames(combined_cm))
cm_df$Actual <- factor(label_map[as.character(cm_df$Actual)], levels = rev(label_map))

# --- Plot with row totals in y-axis (Actual) labels ---
ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), color = "black", size = 4) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (VIs Only)",
    x = "Predicted Class",
    y = "Actual Class",
    fill = "Percentage"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0)
  )


print(cm_percent)
