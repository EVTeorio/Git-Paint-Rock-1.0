

# Load necessary library
library(apaTables)
library(gt)
library(dplyr)

# Define your column names (replace with actual data if needed)
col_names <- c("X", "TileNumber", "SpeciesID", "TreeID", "Boochs", "Boochs2", 
               "CARI", "Carter", "Carter2", "Carter3", "Carter4", "Carter5", 
               "Carter6", "CI", "CI2", "ClAInt", "CRI1", "CRI2", "CRI3", "CRI4", 
               "D1", "D2", "Datt", "Datt2", "Datt3", "Datt4", "Datt5", "Datt6", 
               "DD", "DDn", "DPI", "DWSI4", "EGFN", "EGFR", "EVI", "GDVI2", 
               "GDVI3", "GDVI4", "GI", "Gitelson", "Gitelson2", "GMI1", "GMI2", 
               "GreenNDVI", "Maccioni", "MCARI", "MCARIOSAVI", "MCARI2", 
               "MCARI2OSAVI2", "mND705", "mNDVI", "MPRI", "MSAVI", "mSR", 
               "mSR2", "mSR705", "MTCI", "MTVI", "NDVI", "NDVI2", "NDVI3", 
               "NPCI", "OSAVI", "OSAVI2", "PARS", "PRI", "PRICI2", "PRInorm", 
               "PSND", "PSRI", "PSSR", "RDVI", "REPLE", "REPLi", "SAVI", 
               "SIPI", "SPVI", "SR", "SR1", "SR2", "SR3", "SR4", "SR5", "SR6", 
               "SR7", "SR8", "SRPI", "SumDr1", "SumDr2", "TCARI", "TCARIOSAVI", 
               "TCARI2", "TCARI2OSAVI2", "TGI", "TVI", "Vogelmann", "Vogelmann2", 
               "Vogelmann3", "Vogelmann4", "PAD_0_5_off", "PAD_10_15_off", 
               "PAD_15_20_off", "PAD_20_25_off", "PAD_25_30_off", 
               "PAD_30_35_off", "PAD_35_40_off", "PAD_40_45_off", 
               "PAD_45_50_off", "PAD_5_10_off", "PAD_0_5_on", "PAD_10_15_on", 
               "PAD_15_20_on", "PAD_20_25_on", "PAD_25_30_on", "PAD_30_35_on", 
               "PAD_35_40_on", "PAD_40_45_on", "PAD_45_50_on", "PAD_5_10_on", 
               "Seasonal_Occupancy_20_35m")

# Extract Vegetation Indices and LiDAR metrics
vi_names <- col_names[which(col_names == "Boochs"):which(col_names == "Vogelmann4")]
lidar_names <- col_names[grep("^PAD|Seasonal", col_names)]

# Combine into a table
table_data <- data.frame(`Metric Name` = c(vi_names, lidar_names),
  `Metric Type` = c(rep("Vegetation Index", length(vi_names)),
                    rep("LiDAR Metric", length(lidar_names)))
)

# Create APA-style table with gt
apa_table <- table_data %>%
  gt() %>%
  tab_header(
    title = "Vegetation Indices and LiDAR Metrics Used in the Study",
    subtitle = "Metrics categorized by data source"
  ) %>%
  tab_source_note(
    source_note = "Vegetation indices are derived from spectral reflectance. LiDAR metrics begin with PAD and include seasonal canopy occupancy."
  )

# Display the table
print(apa_table)

# Optional: save to HTML or PNG
gtsave(apa_table, "E:/Git Paint Rock 1.0/For Thesis/apa_table.html")
gtsave(apa_table, "apa_table.png")


#############################################################

## Load required libraries
library(dplyr)
library(tibble)

# Original data with Latin names
species_counts <- tibble(
  SpeciesID = c("CACA38", "CAOV2", "FRAMCO", "LIST2", "LITU", "PIEC2", "QUAL", "TIAM"),
  n = c(13, 12, 18, 13, 24, 23, 36, 9),
  ScientificName = c(
    "Carpinus caroliniana",   # CACA38
    "Carya ovata",            # CAOV2
    "Fraxinus americana",     # FRAMCO
    "Liquidambar styraciflua",# LIST2
    "Liriodendron tulipifera",# LITU
    "Pinus echinata",         # PIEC2
    "Quercus alba",           # QUAL
    "Tilia americana"         # TIAM
  )
)

# Create a clean label column
species_table <- species_counts %>%
  mutate(
    Label = paste0(ScientificName, " (", SpeciesID, ")")
  ) %>%
  select(`Scientific Name` = Label, Count = n)

# Print the table
print(species_table, n = Inf)
# Export as CSV using base R
write.csv(species_table, "E:/Git Paint Rock 1.0/Output/Analysis/species_table.csv", row.names = FALSE)
