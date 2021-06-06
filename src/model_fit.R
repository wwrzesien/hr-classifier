svm_model_fn <- function(train_data, formula, hyperparameters) {
  print("Training SVM - start")

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

logistic_regression_model_fn <- function(train_data, formula, hyperparameters) {
   print("Training logisitic regression")
}

xgboost_model_fn <- function(train_data, formula, hyperparameters) {
  print("Training xgboost - start")

  hyperparameters <- cvms::update_hyperparameters(
    objective = "binary:logistic",
    max_depth = 2,
    nround = 2,
    nthread = 2,
    dummy_model = NULL,
    hyperparameters = hyperparameters
  )

  # Check if dummy vars need to be applied 
  if (!is.null(dummy_model)) {
    train_data <- train_data %>% add_dummy_vars(hyperparameters[["dummy_model"]])
  }
  # Extract data defined by simple formula (only wiht "+" operators)
  destructed_formula <- destruct_formula(formula)
  # [, -1] drops firts columns full of ones which is the result of sparse model matrix
  # conversion. Details: https://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html#one-hot-encoding
  data <- sparse.model.matrix(formula, data = train_data)[, -1]
  label <- train_data[[destructed_formula$dependent]]

  xgboost::xgboost(data = data,
                    label = label,
                    nround = hyperparameters[["nround"]],
                    max.depth = hyperparameters[["max_depth"]],
                    nthread = hyperparameters[["nthread"]],
                    objective = hyperparameters[["objective"]])
}

random_forest_model_fn <- function(train_data, formula, hyperparameters) {
   print("Training random forest")
}
