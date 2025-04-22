

# Install required packages if you haven't already
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("readr")

# Load libraries
library(ggplot2)
library(gridExtra)
library(readr)

setwd("E:/Git Paint Rock 1.0/Output/SampleTables")

# Read the CSV file
data <- read_csv("Sunlit_SampleCount.csv")

# Create a table plot using ggplot
table_plot <- tableGrob(data)

# Save the table plot as an image using ggsave
ggsave("E:/Git Paint Rock 1.0/Output/SampleTables/Sunlit.png", plot = table_plot, width = 10, height = 8)
