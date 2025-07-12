
Final_grouped_results <- readRDS("E:/Results/Final_Model.rds")

summary_list <- list()

# Loop over groups and iterations in Final_grouped_results
for (group_name in names(Final_grouped_results)) {
  group_results <- Final_grouped_results[[group_name]]
  
  for (iter_name in names(group_results)) {
    res <- group_results[[iter_name]]
    
    # Extract metrics
    accuracy <- if (!is.null(res$accuracy)) res$accuracy else NA
    macro_f1 <- if (!is.null(res$macro_f1)) res$macro_f1 else NA
    kappa <- if (!is.null(res$kappa)) res$kappa else NA
    
    # Extract per-class F1 and rename columns (remove prefix if any)
    if (!is.null(res$f1_per_class) && length(res$f1_per_class) > 0) {
      class_f1 <- res$f1_per_class
      clean_names <- paste0("F1_", gsub("^Class: ", "", names(class_f1)))
      class_f1 <- setNames(as.numeric(class_f1), clean_names)
    } else {
      class_f1 <- numeric(0)
    }
    
    # Combine into one row
    row <- c(
      Group = group_name,
      Iteration = iter_name,
      Accuracy = accuracy,
      F1_Macro = macro_f1,
      Kappa = kappa,
      class_f1
    )
    
    summary_list[[paste(group_name, iter_name, sep = "_")]] <- as.data.frame(t(row), stringsAsFactors = FALSE)
  }
}

# Combine into a summary dataframe
summary_df <- do.call(rbind, summary_list)

# Convert numeric columns
num_cols <- setdiff(names(summary_df), c("Group", "Iteration"))
summary_df[num_cols] <- lapply(summary_df[num_cols], as.numeric)

# Print
print(summary_df)

##############################################################################
# Group-wise summary stats
model_means <- summary_df %>%
  group_by(Group) %>%
  summarise(
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE), 3,
    SD_Accuracy = sd(Accuracy, na.rm = TRUE), 3,
    Mean_F1_Macro = mean(F1_Macro, na.rm = TRUE), 3,
    SD_F1_Macro = sd(F1_Macro, na.rm = TRUE), 3,
    Mean_Kappa = mean(Kappa, na.rm = TRUE), 3,
    SD_Kappa = sd(Kappa, na.rm = TRUE), 3
  ) %>%
  arrange(Group)

print(model_means)

# Rename groups based on specified mapping
summary_df <- summary_df %>%
  mutate(Group = recode(Group,
                        "VIs_LiDARleafon" = "VI + leafon_PAD",
                        "VIs_LiDARleafoff" = "VI + leafoff_PAD",
                        "VIs_allLiDAR" = "VI + All_PAD"))


##############################################################################
# Accuracy boxplot
ggplot(summary_df %>% distinct(Group, Iteration, Accuracy), 
       aes(x = Group, y = Accuracy)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Accuracy by Group Across Iterations",
       x = "Group", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# F1 Macro boxplot
ggplot(summary_df %>% distinct(Group, Iteration, F1_Macro), 
       aes(x = Group, y = F1_Macro)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Macro F1 by Group Across Iterations",
       x = "Group", y = "F1 Macro") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
  #scale_y_continuous(limits = c(0.2, 1), expand = c(0, 0))

# Kappa boxplot
ggplot(summary_df %>% distinct(Group, Iteration, Kappa), 
       aes(x = Group, y = Kappa)) +
  geom_boxplot(fill = "#f3a683") +
  labs(title = "Kappa Statistic by Group Across Iterations",
       x = "Group", y = "Kappa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##############################################################################

# Ensure numeric columns are treated as such
numeric_cols <- c("Accuracy", "F1_Macro", "F1_ACSA3C", "F1_CACA38", "F1_CAOV2",
                  "F1_FAGR", "F1_FRAMCO", "F1_LIST2", "F1_LITU", "F1_PIEC2",
                  "F1_QUAL", "F1_QURU", "F1_QUSH", "F1_TIAM", "F1_others")
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
# box plot per species/class F1
# Pivot longer for F1 per class, exclude Macro
f1_long <- summary_df %>%
  pivot_longer(cols = starts_with("F1_") & !starts_with("F1_Macro"),
               names_to = "Class", values_to = "F1") %>%
  filter(!is.na(F1))

ggplot(f1_long, aes(x = Class, y = F1, fill = Group)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "F1 Score by Class and Group", x = "Class", y = "F1 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
########################################################################

# Pivot longer for F1 per class, exclude Macro, and remove specific species
f1_long <- summary_df %>%
  pivot_longer(
    cols = starts_with("F1_") & !starts_with("F1_Macro"),
    names_to = "Class", values_to = "F1"
  ) %>%
  filter(!is.na(F1)) %>%
  # Remove "F1_" prefix and filter out unwanted species
  mutate(Class = str_remove(Class, "^F1_")) %>%
  filter(!Class %in% c("ACSA3C", "FAGR", "QURU", "QUSH", "TIAM"))

# Plot
ggplot(f1_long, aes(x = Class, y = F1, fill = Group)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "F1 Score by Class and Group", x = "Class", y = "F1 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

