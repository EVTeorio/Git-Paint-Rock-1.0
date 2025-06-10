
grouped_accuracy_results <- readRDS("E:/Results/Balanced_Grouped_RF_accuracy_only.rds")

summary_list <- list()

for (sample_name in names(grouped_accuracy_results)) {
  sample_results <- grouped_accuracy_results[[sample_name]]
  
  for (model_name in names(sample_results)) {
    result <- sample_results[[model_name]]

    accuracy <- if (!is.null(result$accuracy)) {
      as.numeric(result$accuracy["Accuracy"])
    }

    f1_macro <- if (!is.null(result$f1_macro)) {
      result$f1_macro
    } 
    
    # Extract per-class F1 and rename columns
    if (!is.null(result$f1_by_class) && length(result$f1_by_class) > 0) {
      class_f1_names <- names(result$f1_by_class)
      # Remove "Class: " prefix from class names for cleaner columns
      clean_names <- paste0("F1_", gsub("^Class: ", "", class_f1_names))
      class_f1 <- setNames(as.numeric(result$f1_by_class), clean_names)
    } else {
      class_f1 <- numeric(0)
    }
    
    # Combine into one row
    row <- c(
      Sample = sample_name,
      Model = model_name,
      Accuracy = accuracy,
      F1_Macro = f1_macro,
      class_f1
    )
    
    # Convert row to data.frame and add to list
    summary_list[[paste(sample_name, model_name, sep = "_")]] <- as.data.frame(t(row), stringsAsFactors = FALSE)
  }
}

# Combine all rows into one summary dataframe
summary_df <- do.call(rbind, summary_list)

# Optionally, convert numeric columns to numeric type
num_cols <- setdiff(names(summary_df), c("Sample", "Model"))
summary_df[num_cols] <- lapply(summary_df[num_cols], as.numeric)

# View summary
print(summary_df)

##############################################################################
model_means <- summary_df %>%
  group_by(Model) %>%
  summarise(
    Mean_Accuracy = round(mean(Accuracy, na.rm = TRUE), 3),
    Mean_F1_Macro = round(mean(F1_Macro, na.rm = TRUE), 3)
  ) %>%
  arrange(Model)

print(model_means)

##############################################################################

# Accuracy boxplot across models
ggplot(summary_df %>% distinct(Sample, Model, Accuracy), 
       aes(x = Model, y = Accuracy)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Accuracy by Model Across 100 Samples",
       x = "Model Type", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# F1 boxplot across models
ggplot(summary_df %>% distinct(Sample, Model, F1_Macro), 
       aes(x = Model, y = F1_Macro)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "F1 by Model Across 100 Samples",
       x = "Model Type", y = "F1_Macro") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##############################################################################
# box plot perSpecies f1
f1_long <- summary_df %>%
  pivot_longer(cols = starts_with("F1_") & !starts_with("F1_Macro"),
               names_to = "Class", values_to = "F1") %>%
  filter(!is.na(F1))

ggplot(f1_long, aes(x = Class, y = F1, fill = Model)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "F1 Score by Class and Model", x = "Class", y = "F1 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################

output_dir <- "E:/Thesis_Final_Data/Analysis/ConfusionMatrixes"

for (sample_name in names(grouped_accuracy_results)) {
  for (model_name in names(grouped_accuracy_results[[sample_name]])) {
    result <- grouped_accuracy_results[[sample_name]][[model_name]]
    
    # Extract confusion matrix table
    cm_table <- result$confusion$table
    cm_file <- file.path(output_dir, paste0(sample_name, "_", model_name, "4Species_Balanced_confusion.csv"))
    write.csv(as.table(cm_table), cm_file)
  }
}
############################################################
#############################################################
print(cm_group)

