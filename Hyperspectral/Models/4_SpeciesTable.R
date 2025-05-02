
# Extract class levels from the ranger model
class_levels <- rf_mod$forest$levels  # These are the levels of the response factor

# Assign numeric values to each class (e.g., 1, 2, 3...)
class_ids <- seq_along(class_levels)

# Create a lookup table
class_lookup <- data.frame(
  class_id = class_ids,
  class_name = class_levels,
  stringsAsFactors = FALSE
)

# View the lookup table
print(class_lookup)

# Save to CSV for later use
write.csv(class_lookup, "E:/Git Paint Rock 1.0/Hyperspectral/Models/class_lookup_table.csv", row.names = FALSE)

# Save the model to an RDS file
saveRDS(rf_mod, file = "E:/Git Paint Rock 1.0/Hyperspectral/Models/rf_model_speciesID.rds")
