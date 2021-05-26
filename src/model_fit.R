svm_model_fn <- function(data_train, formula, hyperparameters) {
  svm(formula=formula, 
      data=df,
      type='C-classification',
      kernel='radial',
      probability=TRUE)
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

model_fit <- function(model_name, data_train, formula, hyperparameters) {
  if (model_name == 'svm') {
    return(svm_model_fn())
  } else if (model_name == 'xgboost') {
    return(xgboost_model_fn())
  } else if (model_name == 'logistic-regression') {
    return(logistic_regression_model_fn())
  } else if (model_name == 'random-forest') {
    return(random_forest_model_fn())
  } else {
    stop(cat(paste0("Error: Unsupported model type! Provided model name: ", model_name, ". Supported models: svm, xgboost, logistic-regression, random-forest.")))
  }
}