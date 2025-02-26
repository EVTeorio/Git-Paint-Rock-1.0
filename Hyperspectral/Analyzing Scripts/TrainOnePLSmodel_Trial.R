


library(beepr)


# Read in data
spec_chem_canopy <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_masked_5nm.csv")
colnames(spec_chem_canopy)

canopy_VIs <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_VIs_5nm.csv") 
canopy_VIs <- canopy_VIs %>%
  select(-X398)

# Make a list of band names for data filtering later
band_names <- colnames(spec_chem_canopy[, 5:ncol(spec_chem_canopy)])
VI_names <- colnames(canopy_VIs[, 5:ncol(canopy_VIs)])

# Join spectra with vegetation indices and Set the dataframe name to match the one used in the workflow
spec_chem_canopy <- spec_chem_canopy %>% 
  inner_join(canopy_VIs, by = c( "X" = "X", "TileNumber" = "TileNumber", "SpeciesID" = "SpeciesID", "TreeID" = "TreeID"), keep = FALSE)


# Set seed for stable cal/val split
set.seed(1234)

# Check structure of the first few columns (TreeID, SpeciesID, etc.)
str(spec_chem_canopy[, 1:4])


# Set the response variable for modeling
className <- "SpeciesID"  # Adjust this variable as per the required response

# Filter data to include rows with the response variable (SpeciesID) Change datset here
spec_chem_canopy_n25 <- canopy_VIs[!is.na(spec_chem_canopy[[className]]), ] %>%
  subset(TreeID != "not sampled") %>%
  mutate(SpeciesID = as.factor(SpeciesID),
         TreeID = as.factor(TreeID)) %>%
  group_by(TileNumber, TreeID, SpeciesID) %>%
  slice_sample(n = 80, replace = FALSE)

# Display counts for each group (e.g., Site, TreeID, SpeciesID)
spec_chem_canopy_n25 %>%
  group_by(TileNumber, TreeID, SpeciesID, eval(parse(text = className))) %>%
  tally() %>%
  print(n = 100)

# Check unique values of the response variable
unique(spec_chem_canopy_n25[[className]])

# Create a test and train split
inTrain <- caret::createDataPartition(
  y = spec_chem_canopy_n25[[className]],
  p = 0.7,
  list = FALSE
)

# Training and testing data subsets (only using TreeID and SpeciesID)
training <- spec_chem_canopy_n25[inTrain, ]

testing <- spec_chem_canopy_n25[-inTrain, ]

# Ranger models (Random Forest)
n <- 1000  # Number of trees
rf_mod <- ranger::ranger(as.formula(paste(className, "~ .")),
                         data = training, num.trees = n)

# Make predictions on the test data
rf_mod_pred <- predict(rf_mod, testing)

