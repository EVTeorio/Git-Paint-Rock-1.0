

summary_list <- list()

# Loop over groups and iterations in Final_grouped_results
for (group_name in names(Final_grouped_results)) {
  group_results <- Final_grouped_results[[group_name]]
  
  for (iter_name in names(group_results)) {
    res <- group_results[[iter_name]]
    
    # Extract metrics
    accuracy <- if (!is.null(res$accuracy)) res$accuracy else NA
    macro_balacc <- if (!is.null(res$macro_balanced_accuracy)) res$macro_balanced_accuracy else NA
    
    # Extract per-class balanced accuracy and rename columns (remove prefix if any)
    if (!is.null(res$balanced_accuracy_per_class) && length(res$balanced_accuracy_per_class) > 0) {
      class_balacc <- res$balanced_accuracy_per_class
      clean_names <- paste0("Balanced_Accuracy_", gsub("^Class: ", "", names(class_balacc)))
      class_balacc <- setNames(as.numeric(class_balacc), clean_names)
    } else {
      class_balacc <- numeric(0)
    }
    
    # Combine into one row
    row <- c(
      Group = group_name,
      Iteration = iter_name,
      Accuracy = accuracy,
      Balanced_Accuracy_Macro = macro_balacc,
      class_balacc
    )
    
    # Convert row to data.frame and add to list
    summary_list[[paste(group_name, iter_name, sep = "_")]] <- as.data.frame(t(row), stringsAsFactors = FALSE)
  }
}

# Combine all rows into one summary dataframe
summary_df <- do.call(rbind, summary_list)

# Convert numeric columns to numeric type
num_cols <- setdiff(names(summary_df), c("Group", "Iteration"))
summary_df[num_cols] <- lapply(summary_df[num_cols], as.numeric)

# Print summary
print(summary_df)

##############################################################################
# Calculate mean accuracy and macro Balanced Accuracy per group
model_means <- summary_df %>%
  group_by(Group) %>%
  summarise(
    Mean_Accuracy = round(mean(Accuracy, na.rm = TRUE), 3),
    Median_Accuracy = round(median(Accuracy, na.rm = TRUE), 3),
    Mean_Balanced_Accuracy_Macro = round(mean(Balanced_Accuracy_Macro, na.rm = TRUE), 3),
    Median_Balanced_Accuracy_Macro = round(median(Balanced_Accuracy_Macro, na.rm = TRUE), 3)
  ) %>%
  arrange(Group)

print(model_means)

##############################################################################

# Accuracy boxplot across groups
ggplot(summary_df %>% distinct(Group, Iteration, Accuracy), 
       aes(x = Group, y = Accuracy)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Accuracy by Group Across Iterations",
       x = "Group", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Balanced Accuracy boxplot across groups
ggplot(summary_df %>% distinct(Group, Iteration, Balanced_Accuracy_Macro), 
       aes(x = Group, y = Balanced_Accuracy_Macro)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Macro Balanced Accuracy by Group Across Iterations",
       x = "Group", y = "Balanced Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
`##############################################################################

# Ensure numeric columns are treated as such
numeric_cols <- c("Accuracy", "Balanced_Accuracy_Macro", "Balanced_Accuracy_ACSA3C", "Balanced_Accuracy_CACA38", "Balanced_Accuracy_CAOV2",
                  "Balanced_Accuracy_FAGR", "Balanced_Accuracy_FRAMCO", "Balanced_Accuracy_LIST2", "Balanced_Accuracy_LITU", "Balanced_Accuracy_PIEC2",
                  "Balanced_Accuracy_QUAL", "Balanced_Accuracy_QURU", "Balanced_Accuracy_QUSH", "Balanced_Accuracy_TIAM", "Balanced_Accuracy_others")
summary_df[numeric_cols] <- lapply(summary_df[numeric_cols], as.numeric)

# Summarize mean and standard deviation by Group
summary_stats <- summary_df %>%
  group_by(Group) %>%
  summarise(across(all_of(numeric_cols),
                   list(mean = ~mean(. , na.rm = TRUE),
                        sd = ~sd(. , na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# View result
print(summary_stats, n = Inf, width = Inf)


##############################################################################
# Box plot per species/class Balanced Accuracy
# Pivot longer for balanced accuracy per class, exclude Macro
balacc_long <- summary_df %>%
  pivot_longer(cols = starts_with("Balanced_Accuracy_") & !starts_with("Balanced_Accuracy_Macro"),
               names_to = "Class", values_to = "Balanced_Accuracy") %>%
  filter(!is.na(Balanced_Accuracy))

ggplot(balacc_long, aes(x = Class, y = Balanced_Accuracy, fill = Group)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "Balanced Accuracy by Class and Group", x = "Class", y = "Balanced Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
