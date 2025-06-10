
# Read in data
PRstem <- read.csv("E:/BASE_PR_20/Updated Census/ALLstems.csv")

# Ensure crown.position is numeric
PRstem$crown.position <- as.numeric(PRstem$crown.position)

# Remove stems with crown.position == 3 and dbh.2024 <= 400
subset_dbh_3 <- PRstem[!(PRstem$crown.position == 3 & PRstem$dbh.2024 <= 400), ]

# From this filtered set, get the 3–5 and 4–5 subsets
subset_3_5 <- PRstem[PRstem$crown.position %in% 3:5, ]
subset_4_5 <- subset_dbh_3_5[subset_dbh_3_5$crown.position %in% 4:5, ]

# Count by species
count_3_5 <- table(subset_3_5$sp)
count_4_5 <- table(subset_4_5$sp)

# Combine into one data frame
all_species <- union(names(count_3_5), names(count_4_5))
crown_counts <- data.frame(
  sp = all_species,
  count_3_5 = as.numeric(count_3_5[match(all_species, names(count_3_5))]),
  count_4_5 = as.numeric(count_4_5[match(all_species, names(count_4_5))])
)

# Replace NAs with 0
crown_counts[is.na(crown_counts)] <- 0

# View result
print(crown_counts)