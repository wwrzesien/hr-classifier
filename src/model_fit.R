svm_model_fn <- function(train_data, formula, hyperparameters) {
  print("Training SVM - start")

  hyperparameters <- cvms::update_hyperparameters(
    kernel = "radial",
    cost = 1,
    hyperparameters = hyperparameters
  )
  
  # ROSE balancing
  train_data <-ROSE(formula , data = train_data)$data
  
  # Weight balancing
  weights = c("0"= nrow(train_folded) / (sum(train_folded$target == "0") *2),
              "1"=nrow(train_folded) / (sum(train_folded$target == "1") *2))
  
  e1071::svm(formula = formula, 
              data = train_data,
              type = 'C-classification',
              # class.weights = weights,
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
  
  # ROSE balancing
  train_data <-ROSE(formula , data = train_data)$data
  
  # Weight balancing. Error: variable lengths differ (found for '(weights)')
  model_weights <- ifelse(train_data$target == "0",
                          nrow(train_data) / (sum(train_data$target == "0") * 2),
                          nrow(train_data) / (sum(train_data$target == "1") * 2))

  stats::glm(
    formula = formula,
    data = train_data,
    # weights = model_weights,
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
    dummy_model = NULL,
    hyperparameters = hyperparameters
  )
  
  # ROSE balancing
  train_data <-ROSE(formula , data = train_data)$data

  # Check if dummy vars need to be applied 
  if (!is.null(hyperparameters[["dummy_model"]])) {
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
                    # scale_pos_weight = sum(train_data$target == 1)/sum(train_data$target == 0)
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
  
  # ROSE balancing
  train_data <-ROSE(formula , data = train_data)$data
  
  # Weight balancing
  weights = c("0"= nrow(train_folded) / (sum(train_folded$target == "0") *2),
              "1"= nrow(train_folded) / (sum(train_folded$target == "1") *2))
  
  randomForest(
    formula = formula,
    data = train_data,
    # classwt = weights,
    ntree = hyperparameters[["ntree"]]
  )
}
