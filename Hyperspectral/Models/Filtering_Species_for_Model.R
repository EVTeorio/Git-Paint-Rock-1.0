

# Read in data
species_filter <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/NewCanopiesMD_Sunlit.csv")


species_filter <- tree_image_spectra_VIs_bind %>%
  filter(`SpeciesID` %in% c("ACNE2", "CACA38", "CELA", "DIVI5", "FRAMCO", "JUNI",
                            "LITU", "PIEC2", "QUAL", "QUSH", "TIAM"))

spec_chem_canopy <- species_filter
