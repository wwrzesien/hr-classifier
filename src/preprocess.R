preprocess_fn <- function(train_data, test_data, formula, hyperparameters) {
  
  # Standardize the training_hours column 
  
  # Get the mean and standard deviation from the train_data
  mean_th <- mean(train_data[["training_hours"]])
  sd_th <- sd(train_data[["training_hours"]])

  # Standardize both train_data and test_data
  train_data[["training_hours"]] <- (train_data[["training_hours"]] - mean_th) / sd_th
  test_data[["training_hours"]] <- (test_data[["training_hours"]] - mean_th) / sd_th

  # Create data frame with applied preprocessing parameters
  preprocess_parameters <- data.frame(
    "Measure" = c("Mean", "SD"),
    "training_hours" = c(mean_th, sd_th)
  )
  
  # Return list with these names
  list("train" = train_data,
       "test" = test_data,
       "parameters" = preprocess_parameters)
}
