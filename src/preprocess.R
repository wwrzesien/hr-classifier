preprocess_fn <- function(train_data, test_data, formula, hyperparameters) {
  
  # Normalize the training_hours column 
  normalization_model = create_normalize_data_model(train_base, c("training_hours"))

  # Preprocessing pipeline
  train_data <- train_data %>% normalize_data(normalization_model)
  test_data <- test_data %>% normalize_data(normalization_model)

  # Create data frame with applied preprocessing parameters
  preprocess_parameters <- data.frame(normalization_model = normalization_model)
  
  # Return list with these names
  list("train" = train_data,
       "test" = test_data,
       "parameters" = preprocess_parameters)
}
