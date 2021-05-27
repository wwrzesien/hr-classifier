svm_model_fn <- function(train_data, formula, hyperparameters) {
  hyperparameters <- cvms::update_hyperparameters(
    kernel = "radial",
    cost = 1,
    hyperparameters = hyperparameters
  )
  
  e1071::svm(formula = formula, 
              data = train_data,
              type = 'C-classification',
              kernel = hyperparameters[["kernel"]],
              cost = hyperparameters[["cost"]],
              probability = TRUE)
}

logistic_regression_model_fn <- function(data_train, formula, hyperparameters) {
   print("Training logisitic regression")
}

xgboost_model_fn <- function(data_train, formula, hyperparameters) {
   print("Training xgboost")
}

random_forest_model_fn <- function(data_train, formula, hyperparameters) {
   print("Training random forest")
}

model_fit <- function(model_name, train_data, formula, hyperparameters = list()) {
  if (model_name == 'svm')
    return(svm_model_fn(train_data, formula, hyperparameters))
  else if (model_name == 'xgboost')
    return(xgboost_model_fn())
  else if (model_name == 'logistic-regression')
    return(logistic_regression_model_fn())
  else if (model_name == 'random-forest')
    return(random_forest_model_fn())
  else
    stop(cat(paste0("Error: Unsupported model type! Provided model name: ", model_name, ". Supported models: svm, xgboost, logistic-regression, random-forest.")))
}
