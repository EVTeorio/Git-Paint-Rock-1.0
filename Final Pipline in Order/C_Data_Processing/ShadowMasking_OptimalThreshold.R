

# Load required libraries
library(terra)
library(tidyverse)
library(pROC)

# 1. Load Raster and Vector Data ------------------------------------------
veg_raster <- rast("E:/Vegetaion Indices Images/raw_23392_rd_rf_or_VI.envi")
shadow_poly <- vect("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/shadow_training.shp")
sunlit_poly <- vect("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/sunlit_training.shp")

# 2. Assign Names to Raster Bands ------------------------------------------
band_names <- c("Boochs", "Boochs2", "CARI", "Carter", "Carter2", "Carter3",
                "Carter4", "Carter5", "Carter6", "CI", "CI2", "ClAInt",
                "CRI1", "CRI2", "CRI3", "CRI4", "D1", "D2", "Datt", "Datt2",
                "Datt3", "Datt4", "Datt5", "Datt6", "DD", "DDn", "DPI", "DWSI4",
                "EGFN", "EGFR", "EVI", "GDVI2", "GDVI3", "GDVI4", "GI",
                "Gitelson", "Gitelson2", "GMI1", "GMI2", "GreenNDVI", "Maccioni",
                "MCARI", "MCARIOSAVI", "MCARI2", "MCARI2OSAVI2", "mND705",
                "mNDVI", "MPRI", "MSAVI", "mSR", "mSR2", "mSR705", "MTCI",
                "MTVI", "NDVI", "NDVI2", "NDVI3", "NPCI", "OSAVI", "OSAVI2",
                "PARS", "PRI", "PRICI2", "PRInorm", "PSND", "PSRI", "PSSR",
                "RDVI", "REPLE", "REPLi", "SAVI", "SIPI", "SPVI", "SR", "SR1",
                "SR2", "SR3", "SR4", "SR5", "SR6", "SR7", "SR8", "SRPI",
                "SumDr1", "SumDr2", "TCARI", "TCARIOSAVI", "TCARI2",
                "TCARI2OSAVI2", "TGI", "TVI", "Vogelmann", "Vogelmann2",
                "Vogelmann3", "Vogelmann4")

names(veg_raster) <- band_names

# 3. Extract Values --------------------------------------------------------
shadow_vals <- extract(veg_raster, shadow_poly) %>% mutate(class = "shadow")
sunlit_vals <- extract(veg_raster, sunlit_poly) %>% mutate(class = "sunlit")

# Combine both
training_data <- bind_rows(shadow_vals, sunlit_vals) %>% select(-ID)

# 4. Pivot to Long Format for Analysis ------------------------------------
long_vals <- training_data %>%
  pivot_longer(cols = -class, names_to = "index", values_to = "value") %>%
  drop_na()

# 5. Compute ROC AUC for Each Index ----------------------------------------
roc_scores <- long_vals %>%
  group_by(index) %>%
  summarise(
    auc = tryCatch({
      roc_obj <- roc(response = class, predictor = value, levels = c("shadow", "sunlit"), quiet = TRUE)
      as.numeric(auc(roc_obj))  # Convert from auc object to numeric
    }, error = function(e) NA_real_)
  ) %>%
  arrange(desc(auc))

# Show top-performing indices
print(head(roc_scores, 10))

# 6. Determine Optimal Threshold on Best Index -----------------------------
best_index <- roc_scores$index[1]
best_vals <- long_vals %>% filter(index == best_index)

# Compute optimal threshold using ROC coords
roc_obj <- roc(response = best_vals$class, predictor = best_vals$value, levels = c("shadow", "sunlit"))
opt_threshold <- coords(roc_obj, "best", ret = "threshold")

cat("Best index for separating shadow/sunlit:", best_index, "\n")
#############################################################################
#masking image
# Apply threshold
best_band <- veg_raster[[best_index]]
shadow_mask <- best_band > opt_threshold
shadow_mask_raster <- shadow_mask[[1]]
plot(shadow_mask_raster)
writeRaster(shadow_mask_raster, "E:/Git Paint Rock 1.0/Output/masked_23392.tif", overwrite = TRUE)
#########################for figure##################
HSI <- rast("E:/Hyperspec Images/raw_23392_rd_rf_or")
masked_shadow_image <- crop(HSI, shadow_mask_raster, mask=TRUE)
plot(masked_shadow_image)
writeRaster(shadow_mask_raster, "C:/Users/PaintRock/Desktop/HSI_masked_23392.tif", overwrite = TRUE)
help("mask")
