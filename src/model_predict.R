model_predict <- function(test_data, model, formula, hyperparameters, train_data) {
  predictions <- predict(object = model,
          newdata = test_data,
          allow.new.levels = TRUE,
          probability = TRUE)

  # Extract probabilities
  probabilities <- dplyr::as_tibble(attr(predictions, "probabilities"))
  
  # Return second column
  return(probabilities[[2]])
}