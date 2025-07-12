
library(dplyr)


# Read in data
PRstem <- read.csv("E:/BASE_PR_20/Updated Census/ALLstems.csv")
str(PRstem)
# 17 Ha only
PRstem$crown.position <- as.numeric(PRstem$crown.position)
PRstem <- PRstem[PRstem$x >= -86.30800, ]

# Count species
species_counts <- PRstem %>%
  count(sp)

# Remove stems with crown.position == 3 and dbh.2024 <= 400
subset_dbh_3 <- PRstem[!(PRstem$crown.position == 3 & PRstem$DBH.2024 <= 400), ]


# From this filtered set, get the 3–5 and 4–5 subsets
subset_3_5 <- subset_dbh_3[subset_dbh_3$crown.position %in% 3:5, ]
subset_4_5 <- subset_dbh_3[subset_dbh_3$crown.position %in% 4:5, ]

species_counts <- subset_3_5 %>%
  count(sp)

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


# View result
print(crown_counts)
