

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(beepr)
beep(8)


# Read in the CSV file
data <- read.csv(
  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Raw_PixelSpectra_Trial.csv")

# Remove rows with NA values
df <- na.omit(data)

# Function to calculate z-scores, identify anomalies, and remove the rows with anomalies
detect_and_remove_anomalies <- function(df, threshold = 5) {
  # Select numeric columns from the dataframe
  numeric_cols <- df %>%
    select(where(is.numeric)) %>%
    colnames()
  
  # Initialize lists to store results
  anomalies_list <- list()
  anomalies_count <- data.frame(Column = character(), AnomalyCount = integer(), stringsAsFactors = FALSE)
  all_anomalies <- integer(0)  # Vector to store all row indices with anomalies
  
  # Loop over each numeric column and calculate anomalies
  for (col in numeric_cols) {
    # Calculate z-scores for the column
    z_scores <- scale(df[[col]], center = TRUE, scale = TRUE)
    
    # Identify anomalies where absolute z-score exceeds threshold
    anomalies <- which(abs(z_scores) > threshold)
    
    # If anomalies are found, store them and update count
    if (length(anomalies) > 0) {
      anomalies_list[[col]] <- df[anomalies, c("TileNumber", "SpeciesID", "TreeID", col)]
      anomalies_count <- rbind(anomalies_count, data.frame(Column = col, AnomalyCount = length(anomalies)))
      all_anomalies <- unique(c(all_anomalies, anomalies))  # Add to the list of all anomalies
    }
  }
  
  # Remove rows with anomalies from the dataframe
  df_cleaned <- df[-all_anomalies, ]
  
  # Return both the cleaned dataframe and the anomalies information
  return(list(CleanedData = df_cleaned, Anomalies = anomalies_list, AnomalyCounts = anomalies_count))
}

# Function to display the number of anomalies for each column and plot histograms
count_and_plot_anomalies <- function(anomalies_count, anomalies_list, df) {
  # Display anomaly counts
  if (nrow(anomalies_count) > 0) {
    cat("Number of anomalies detected for each column:\n")
    print(anomalies_count)
  } else {
    cat("No anomalies detected in any column.\n")
  }
  
  # Plot histograms for each column with anomalies highlighted
  for (col_name in names(anomalies_list)) {
    # Plot histogram of the column
    plot_data <- df[[col_name]]
    anomalies <- anomalies_list[[col_name]]
    
    # Create the ggplot for the histogram
    p <- ggplot(df, aes(x = plot_data)) +
      geom_histogram(binwidth = diff(range(plot_data)) / 30, fill = "lightblue", color = "black", alpha = 0.6) +
      geom_point(data = anomalies, aes(x = anomalies[[col_name]], y = rep(0, length(anomalies[[col_name]]))), color = "red", size = 2) +
      labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency") +
      theme_minimal()
    
    print(p)
  }
}

# Apply the function on your dataframe
results <- detect_and_remove_anomalies(df)

# Display the number of anomalies for each column and plot the histograms
count_and_plot_anomalies(results$AnomalyCounts, results$Anomalies, df)

# The cleaned dataframe is now available
data_clean <- results$CleanedData

write.csv(data_clean,"C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_clean_speclib.csv")



