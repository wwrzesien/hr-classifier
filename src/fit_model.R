train_svm <- function(formula, df, kernel='radial') {
   return(svm(formula=formula, 
              data=df,
              type='C-classification',
              kernel==kernel,
              probability=TRUE))
}

train_logistic_regression <- function() {
   print("Training logisitic regression")
}

train_xgboost <- function() {
   print("Training xgboost")
}

train_random_forest <- function() {
   print("Training random forest")
}

fit_model <- function(model_name) {
  if (model_name == 'svm') {
    return(train_svm())
  } else if (model_name == 'xgboost') {
    return(train_xgboost())
  } else if (model_name == 'logistic-regression') {
    return(train_logistic_regression())
  } else if (model_name == 'random-forest') {
    return(train_random_forest())
  } else {
    stop(cat(paste0("Error: Unsupported model type! Provided model name: ", model_name, ". Supported models: svm, xgboost, logistic-regression, random-forest.")))
  }
}