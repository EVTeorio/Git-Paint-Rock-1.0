
library(ggplot2)
library(dplyr)
library(tidyr)

Model_acc <- read.csv("E:/Thesis_Final_Data/Analysis/model_summary_metrics.csv")

# Summarize the data per model
performance_data <- Model_acc %>%
  select(Sample, Model, Accuracy = Accuracy.Accuracy, F1_Macro) %>%
  mutate(Model = factor(Model, levels = c("VIs_only", "VIs_LiDARleafon",
                                          "VIs_LiDARleafoff", "VIs_allLiDAR")))
# Plot Accuracy
ggplot(performance_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Accuracy Across Models", y = "Accuracy", x = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Macro F1
ggplot(performance_data, aes(x = Model, y = F1_Macro, fill = Model)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Macro F1 Score Across Models", y = "Macro F1", x = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########
# Select F1 columns for each species and reshape
f1_long <- Model_acc %>%
  select(Model, starts_with("F1_")) %>%
  pivot_longer(cols = starts_with("F1_"),
               names_to = "Species",
               values_to = "F1_Score") %>%
  mutate(Species = gsub("F1_", "", Species),
         Model = factor(Model, levels = c("VIs_only", "VIs_LiDARleafon",
                                          "VIs_LiDARleafoff", "VIs_allLiDAR")))

# Calculate mean F1 score per species and model
f1_summary <- f1_long %>%
  group_by(Species, Model) %>%
  summarise(Mean_F1 = mean(F1_Score), .groups = "drop")

# Plot heatmap
ggplot(f1_summary, aes(x = Model, y = Species, fill = Mean_F1)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", name = "Mean F1") +
  theme_minimal() +
  labs(title = "Mean F1 Score by Species and Model", x = "Model", y = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

