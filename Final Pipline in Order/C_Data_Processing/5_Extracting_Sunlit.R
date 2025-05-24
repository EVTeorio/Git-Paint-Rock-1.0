

# Load necessary libraries
library(dplyr)
library(tidyr)
library(beepr)
beep()
###################### masking shadow pixels ###################################
data_clean <- read.csv("E:/Thesis_Final_Data/ALLmetrics_clean.csv")
# Filter rows 
filtered_data <- data_clean[data_clean$ClAInt >= 2.26424, ]

write.csv(filtered_data,"E:/Thesis_Final_Data/ALLmetrics_clean_sunlit.csv")

################################################################################
#removing LiDAR Metrics with little information
Columns_rem <- filtered_data[, !names(filtered_data) %in% c(
  "PAD_40_45_off", "PAD_45_50_off", "PAD_40_45_on", "PAD_45_50_on"
)]

write.csv(Columns_rem,"E:/Thesis_Final_Data/ALLmetrics_EndALL_BeALL.csv")
beep(3)
