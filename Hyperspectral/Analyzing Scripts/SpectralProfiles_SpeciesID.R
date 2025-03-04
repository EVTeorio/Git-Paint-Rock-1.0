

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(beepr)

# Read in data
df <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Outliers_Removed.csv")

# Gather all spectral columns into long format
df_long <- df %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Wavelength", 
               values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X", "", Wavelength),
         Wavelength = gsub(".nm", "", Wavelength), # Clean up the wavelength names (remove 'X')
         Wavelength = as.numeric(Wavelength)) %>%
  filter(!is.na(Wavelength)) %>%  # Remove any rows where Wavelength is NA
  filter(Wavelength != 0.1)  # Filter out rows with Wavelength value of 0.1 (or any other invalid value)

# Define a function to generate and save plots for each group (either SpeciesID or TreeID)
generate_plot <- function(df_long, group_col, output_path_prefix) {
  
  # Calculate summary statistics by Wavelength for the group
  summary_stats <- df_long %>%
    group_by(Wavelength, !!sym(group_col)) %>%
    summarise(
      Median_Reflectance = median(Reflectance, na.rm = TRUE),
      Max_Reflectance = max(Reflectance, na.rm = TRUE),
      Min_Reflectance = min(Reflectance, na.rm = TRUE),
      Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875, na.rm = TRUE),
      Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125, na.rm = TRUE),
      Upper_Reflectance = quantile(Reflectance, probs = 0.95, na.rm = TRUE),
      Lower_Reflectance = quantile(Reflectance, probs = 0.05, na.rm = TRUE)
    )
  
  # Loop over each unique group and generate a plot
  unique_groups <- unique(df_long[[group_col]])
  
  for (group in unique_groups) {
    group_data <- summary_stats %>%
      filter(!!sym(group_col) == group)
    
    p <- ggplot(group_data, aes(x = Wavelength)) +
      
      # Shaded region for the first quantile (between 12.5% and 87.5%, light grey)
      geom_ribbon(aes(ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), 
                  fill = "gray", alpha = 0.75) +
      
      # Shaded region for the second quantile (between 5% and 95%, darker grey)
      geom_ribbon(aes(ymin = Lower_Reflectance, ymax = Upper_Reflectance), 
                  fill = "gray", alpha = 0.25) +
      
      # Solid black line for the median reflectance
      geom_line(aes(y = Median_Reflectance), color = "black", linewidth = 1) +
      
      # Labels and theme adjustments
      labs(title = paste("Reflectance Across Wavelengths for", group),
           x = "Wavelength (nm)",
           y = "Reflectance") +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Display the plot
    print(p)
    
    # Define the output file path based on the group
    output_path <- paste0(output_path_prefix, "_", group, ".png")
    
    # Save the plot to the specified location
    ggsave(output_path, plot = p, width = 10, height = 6)
  }
}

# Specify the output path prefix
output_path_prefix_species <- "E:/Git Paint Rock 1.0/Output/reflectance_species"
output_path_prefix_tree <- "E:/Git Paint Rock 1.0/Output/reflectance_tree"

# Generate plots for each SpeciesID
generate_plot(df_long, "SpeciesID", output_path_prefix_species)

# Generate plots for each TreeID
generate_plot(df_long, "TreeID", output_path_prefix_tree)
