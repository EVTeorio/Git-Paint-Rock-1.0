
library(dplyr)

spectral_df <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/NewCanopiesMD_Clean.csv")

spectral_df <- spec_chem_canopy

# Assuming 'spectral_df' is the dataframe produced by the extract_spectral_data function
unique_combinations <- spectral_df %>%
  select(TileNumber, SpeciesID, TreeID) %>%
  distinct()

# Get the count of individual canopies per SpeciesID (count unique TreeID per SpeciesID)
canopy_count_per_species <- spectral_df %>%
  group_by(SpeciesID) %>%
  summarise(NumCanopies = n_distinct(TreeID))

# Count the number of rows for each unique combination of TileNumber, SpeciesID, and TreeID
row_count_per_combination <- spectral_df %>%
  group_by(TileNumber, SpeciesID, TreeID) %>%
  summarise(RowCount = n())

library(ggplot2)


# You can now view the unique combinations in the 'unique_combinations' dataframe
print(unique_combinations)

# Optionally, write the unique combinations to a CSV file
write.csv(unique_combinations, file.path("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Unique_Combinations.csv"),
          row.names = FALSE)