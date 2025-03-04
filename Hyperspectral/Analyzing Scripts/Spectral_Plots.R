

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# read in data
df <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Outliers_Removed.csv")


# Gather all spectral columns into long format
df_long <- df %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Wavelength", 
               values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X", "", Wavelength),
         Wavelength = gsub(".nm", "", Wavelength), # Clean up the wavelength names (remove 'X')
         Wavelength = as.numeric(Wavelength)) %>%
  filter(!is.na(Wavelength))  # Remove any rows where Wavelength is NA


# Calculate summary statistics by Wavelength
summary_stats <- df_long %>%
  group_by(Wavelength) %>%
  summarise(
    Median_Reflectance = median(Reflectance, na.rm = TRUE),
    Max_Reflectance = max(Reflectance, na.rm = TRUE),
    Min_Reflectance = min(Reflectance, na.rm = TRUE),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875, na.rm = TRUE),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125, na.rm = TRUE),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95, na.rm = TRUE),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05, na.rm = TRUE)
  )

p <- ggplot(summary_stats, aes(x = Wavelength)) +
  
  # Shaded region for the first quantile (between 12.5% and 87.5%, light grey)
  geom_ribbon(aes(ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), 
              fill = "gray", alpha = 0.75) +
  
  # Shaded region for the second quantile (between 5% and 95%, darker grey)
  geom_ribbon(aes(ymin = Lower_Reflectance, ymax = Upper_Reflectance), 
              fill = "gray", alpha = 0.25) +
  
  # Solid black line for the median reflectance
  geom_line(aes(y = Median_Reflectance), color = "black", linewidth = 1)+
  
  # Labels and theme adjustments
  labs(title = "Reflectance Across Wavelengths",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(p)

# Specify the path and filename for saving the plot
output_path <- "E:/Git Paint Rock 1.0/Output/reflectance_outliers_removed.png"

# Save the plot to the specified location
ggsave(output_path, plot = p, width = 10, height = 6)
