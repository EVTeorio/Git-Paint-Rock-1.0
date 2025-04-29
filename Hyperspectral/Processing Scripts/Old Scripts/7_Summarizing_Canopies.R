

tree_image_spectra_VIs_bind <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/MD_VegIndex_5nm.csv")

# Select the columns for summarization (excluding the first three attribute columns)
vi_columns <- names(tree_image_spectra_VIs_bind)[5:ncol(tree_image_spectra_VIs_bind)]  # All vegetation indices columns

# Summarize by TileNumber, SpeciesID, and TreeID, calculating the mean for each vegetation index
mean_vegetation_indices <- tree_image_spectra_VIs_bind %>%
  select(TileNumber, SpeciesID, TreeID, all_of(vi_columns)) %>%
  group_by(TileNumber, SpeciesID, TreeID) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")


write.csv(mean_vegetation_indices,
          "E:/Git Paint Rock 1.0/Output/Summarized_CSV/Summerized_After_VegIndex_Calc.csv")
