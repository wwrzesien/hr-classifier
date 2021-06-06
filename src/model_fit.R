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

lg_model_fn <- function(train_data, formula, hyperparameters) {
   print("Training logisitic regression")
  
  hyperparameters <- cvms::update_hyperparameters(
    family = "binomial",
    hyperparameters = hyperparameters
  )
  
  stats::glm(
    formula = formula,
    data = train_data,
    family = hyperparameters[["family"]]
  )
}

xgboost_model_fn <- function(train_data, formula, hyperparameters) {
  print("Training xgboost - start")

  hyperparameters <- cvms::update_hyperparameters(
    objective = "binary:logistic",
    max_depth = 2,
    nround = 2,
    nthread = 2,
    hyperparameters = hyperparameters
  )

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

forest_model_fn <- function(train_data, formula, hyperparameters) {
   print("Training random forest")
  
  hyperparameters <- cvms::update_hyperparameters(
    ntree = 100,
    hyperparameters = hyperparameters
  )
  
  randomForest(
    formula = formula,
    data = train_data,
    ntree = hyperparameters[["ntree"]]
  )
}

# model_fit <- function(model_name, train_data, formula, hyperparameters = list()) {
#   if (model_name == 'svm')
#     return(svm_model_fn(train_data, formula, hyperparameters))
#   else if (model_name == 'xgboost')
#     return(xgboost_model_fn())
#   else if (model_name == 'logistic-regression')
#     return(logistic_regression_model_fn())
#   else if (model_name == 'random-forest')
#     return(random_forest_model_fn())
#   else
#     stop(cat(paste0("Error: Unsupported model type! Provided model name: ", model_name, ". Supported models: svm, xgboost, logistic-regression, random-forest.")))
# }
