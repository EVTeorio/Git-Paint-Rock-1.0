

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
combined_cm <- combine_confusion_matrices(Final_grouped_results[["VIs_only"]])

# --- Compute row totals and prepare labels for y-axis (Actual) ---
row_totals <- colSums(combined_cm)
actual_labels <- paste0(rownames(combined_cm), " (n = ", row_totals, ")")

# --- Convert to column-wise percentages ---
cm_percent <- prop.table(combined_cm, margin = 2) * 100  # Column-wise percentages
cm_df <- as.data.frame(as.table(cm_percent)) %>%
  rename(Actual = Reference, Predicted = Prediction)

# --- Format percentage labels ---
cm_df$Label <- sprintf("%d%%", round(cm_df$Freq))

# --- Ensure Predicted is in correct order for x-axis (unchanged) ---
# Reorder levels so "others" is last
predicted_levels <- colnames(combined_cm)
if ("others" %in% predicted_levels) {
  predicted_levels <- c(setdiff(predicted_levels, "others"), "others")
}
cm_df$Predicted <- factor(cm_df$Predicted, levels = predicted_levels)

# Reorder so "others" is last in actual (row) labels
actual_levels <- rownames(combined_cm)
if ("others" %in% actual_levels) {
  actual_levels <- c(setdiff(actual_levels, "others"), "others")
}

# Update label map and factor levels
label_map <- setNames(paste0(actual_levels, " (n = ", row_totals[actual_levels], ")"), actual_levels)
cm_df$Actual <- factor(as.character(cm_df$Actual), levels = actual_levels)
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
