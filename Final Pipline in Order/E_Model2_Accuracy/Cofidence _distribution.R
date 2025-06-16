

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)


# Extract only confidence values from all model results
confidence_df <- map_dfr(names(Final_grouped_results), function(group_name) {
  map_dfr(Final_grouped_results[[group_name]], function(iteration_result) {
    tibble(Confidence = iteration_result$confidence)
  }, .id = "Iteration") %>%
    mutate(Group = group_name)
}, .id = "GroupID")


# Plot 1: Density plot of confidence values per group
ggplot(confidence_df, aes(x = Confidence, fill = Group)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribution of Prediction Confidence Across Groups",
       x = "Confidence Score", y = "Density") +
  theme_minimal()

# Optional Plot 2: Boxplot of confidence scores by group
ggplot(confidence_df, aes(x = Group, y = Confidence, fill = Group)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "Confidence Score Distribution by Group",
       x = "Model Input Group", y = "Confidence Score") +
  theme_minimal()+
  scale_y_continuous(limits = c(0.2, .3), expand = c(0, 0))

