
# 5. Predict using the trained Random Forest model (with probability prediction enabled)
rf_mod_pred <- predict(rf_mod, spectral_df, type = "response", probability = TRUE)

