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

lg_predict_fn <- function(test_data, model, formula, hyperparameters, train_data){
  stats::predict(
    object = model,
    newdata = test_data,
    type = "response")
}

xgboost_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
  predict(object = model,
          newdata = sparse.model.matrix(as.formula(formula), data = test_data)[, -1],
          allow.new.levels = TRUE,
          probability = TRUE)
}

