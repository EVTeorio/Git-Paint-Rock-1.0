
#convert the ,xlxs to .csv in excel fisrt
#the filtering steps can be modified or skipped before creating Stem map#

#Load necessary libraries#Required to convert to Stem map
library(dplyr)
library(sf)


#Read the CSV file#Required to convert to Stem map
csv_file <- "E:/BASE_PR_20/Updated Census/larger main stems.csv"  # Replace with your file path
df <- read.csv(csv_file)

# Filter rows where "crown position" is 3, 4, or 5, and "status 2024" is "A"
df_filtered <- df %>%
  filter(`crown.position` %in% c(3, 4, 5), `status.2024` == "A")

# Keep only the desired columns
df_filtered_crown_position <- df_filtered %>%
  select(tag, sp, previous_dbh, lat, long, `DBH.2024`, `status.2024`, `crown.position`, `lean.angle`, `lean.direction`)

# Remove rows with missing lat or long values
df_filtered_crown_position <- df_filtered_crown_position %>%
  filter(!is.na(lat) & !is.na(long))

#Convert to spatial points using 'lat' and 'long'#Required to convert to Stem map
#Replace "df_filtered_crown_position" with "df" if filtering steps are skipped
sf_points <- st_as_sf(df_filtered_crown_position, coords = c("long", "lat"), crs = 4326) #should take less than 5min
plot(sf_points)

# Step 5: Save as a shapefile#Required to convert to Stem map
shapefile_output <- "E:/BASE_PR_20/Updated Census/StemMap_3_25.shp"  # Replace with your desired output file path
st_write(sf_points, shapefile_output)

cat("Shapefile created successfully.")
