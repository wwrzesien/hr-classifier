model_predict <- function(test_data, model, formula, hyperparameters, train_data) {
  hyperparameters <- cvms::update_hyperparameters(
    dummy_model = NULL,
    hyperparameters = hyperparameters
  )
  # Check if dummy vars need to be applied 
  if (!is.null(hyperparameters[["dummy_model"]])) {
    test_data <- test_data %>% add_dummy_vars(hyperparameters[["dummy_model"]])
  }
  
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
  hyperparameters <- cvms::update_hyperparameters(
    dummy_model = NULL,
    hyperparameters = hyperparameters
  )
  # Check if dummy vars need to be applied 
  if (!is.null(hyperparameters[["dummy_model"]])) {
    test_data <- test_data %>% add_dummy_vars(hyperparameters[["dummy_model"]])
  }
  
  predict(object = model,
          newdata = sparse.model.matrix(as.formula(formula), data = test_data)[, -1],
          allow.new.levels = TRUE,
          probability = TRUE)
}

forest_predict_fn <- function(test_data, model, formula, hyperparameters, train_data){
  stats::predict(
    object = model,
    newdata = test_data,
    type = "response")
}

