
install.packages("rcompanion")
library(rcompanion)


# Clean data: remove rows with NA in F1_Macro
df_clean <- summary_df %>% filter(!is.na(F1_Macro))

# Get unique groups
groups <- unique(df_clean$Group)

# Initialize results dataframe
results <- data.frame(Group1 = character(),
                      Group2 = character(),
                      Mean1 = numeric(),
                      Mean2 = numeric(),
                      Mean_Diff = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

# Loop through all unique pairwise combinations and run t-tests
for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    g1 <- groups[i]
    g2 <- groups[j]
    
    data_g1 <- df_clean$F1_Macro[df_clean$Group == g1]
    data_g2 <- df_clean$F1_Macro[df_clean$Group == g2]
    
    ttest <- t.test(data_g1, data_g2)
    
    mean1 <- mean(data_g1)
    mean2 <- mean(data_g2)
    mean_diff <- mean1 - mean2
    
    results <- rbind(results, data.frame(Group1 = g1,
                                         Group2 = g2,
                                         Mean1 = mean1,
                                         Mean2 = mean2,
                                         Mean_Diff = mean_diff,
                                         p_value = ttest$p.value,
                                         stringsAsFactors = FALSE))
  }
}

# Adjust p-values for multiple comparisons (Bonferroni correction)
results$p_adjusted <- p.adjust(results$p_value, method = "bonferroni")

# Print results sorted by adjusted p-value
results <- results %>% arrange(p_adjusted)
print(results)


# Assuming your data frame is called summary_df
# Remove rows with NA in F1_Macro
df_clean <- summary_df %>% filter(!is.na(F1_Macro))

# Run pairwise Wilcoxon tests with Bonferroni correction
wilcox_results <- pairwise.wilcox.test(df_clean$F1_Macro, df_clean$Group,
                                       p.adjust.method = "bonferroni")

# Print the results
print(wilcox_results)
###################################################################3

# Your data
df_clean <- summary_df %>% filter(!is.na(F1_Macro))

# Function to calculate rank-biserial correlation for two groups
calc_rbc_manual <- function(data, group1, group2) {
  group1_vals <- data$F1_Macro[data$Group == group1]
  group2_vals <- data$F1_Macro[data$Group == group2]
  
  # Wilcoxon test, exact = FALSE for large samples
  wtest <- wilcox.test(group1_vals, group2_vals, exact = FALSE)
  
  W <- wtest$statistic  # Wilcoxon W statistic
  n1 <- length(group1_vals)
  n2 <- length(group2_vals)
  
  # Calculate rank-biserial correlation
  rbc <- 1 - (2 * W) / (n1 * n2)
  
  data.frame(
    group1 = group1,
    group2 = group2,
    p_value = wtest$p.value,
    rank_biserial_correlation = rbc
  )
}

# Use this function now
groups <- unique(df_clean$Group)
pairs <- combn(groups, 2, simplify = FALSE)
effect_sizes <- do.call(rbind, lapply(pairs, function(x) calc_rbc_manual(df_clean, x[1], x[2])))

print(effect_sizes)



ggplot(df_clean, aes(x = Group, y = F1_Macro, fill = Group)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  theme_minimal() +
  labs(title = "Distribution of F1_Macro by Group",
       y = "F1_Macro",
       x = "Group") +
  theme(legend.position = "none")
