svm_model_fn <- function(train_data, formula, hyperparameters) {
  print("Training SVM - start")

  hyperparameters <- cvms::update_hyperparameters(
    kernel = "radial",
    cost = 1,
    use_rose = FALSE,
    use_weights = FALSE,
    hyperparameters = hyperparameters
  )
  
  # ROSE balancing
  if (hyperparameters[["use_rose"]]) {
    print("Training SVM - use ROSE")
    train_data <-ROSE(formula , data = train_data)$data
  }
  
  # Weight balancing. 
  model_weights = NULL
  if (hyperparameters[["use_weights"]]) {
    print("Training lSVM - use weights")
    weights <- (train_data)
    model_weights = c("0"=weights$weight_0, "1"=weights$weight_1)
  }
  
  e1071::svm(formula = formula, 
              data = train_data,
              type = 'C-classification',
              class.weights = model_weights,
              kernel = hyperparameters[["kernel"]],
              cost = hyperparameters[["cost"]],
              probability = TRUE)
}

lg_model_fn <- function(train_data, formula, hyperparameters) {
  print("Training logisitic regression - start")
  
  hyperparameters <- cvms::update_hyperparameters(
    family = "binomial",
    use_rose = FALSE,
    use_weights = FALSE,
    dummy_model = NULL,
    hyperparameters = hyperparameters
  )
  
  # ROSE balancing
  if (hyperparameters[["use_rose"]]) {
    print("Training logisitic regression - use ROSE")
    train_data <-ROSE(formula , data = train_data)$data
  }
  
  # Check if dummy vars need to be applied 
  if (!is.null(hyperparameters[["dummy_model"]])) {
    print("Training xgboost - use dummyVars")
    train_data <- train_data %>% add_dummy_vars(hyperparameters[["dummy_model"]])
  }

  # Weight balancing. Error: variable lengths differ (found for '(weights)')
  if (hyperparameters[["use_weights"]]) {
    print("Training logisitic regression - use weights")
    weights <- calc_weights(train_data)

    return(stats::glm(
      formula = formula,
      data = train_data,
      weights = weights$model_weights,
      family = hyperparameters[["family"]]))
  }

  stats::glm(
    formula = formula,
    data = train_data,
    family = hyperparameters[["family"]])
}

xgboost_model_fn <- function(train_data, formula, hyperparameters) {
  print("Training xgboost - start")

  hyperparameters <- cvms::update_hyperparameters(
    objective = "binary:logistic",
    max_depth = 2,
    nround = 2,
    nthread = 2,
    dummy_model = NULL,
    use_rose = FALSE,
    use_weights = FALSE,
    hyperparameters = hyperparameters
  )
  
  # ROSE balancing
  if (hyperparameters[["use_rose"]]) {
    print("Training xgboost - use ROSE")
    train_data <-ROSE(formula , data = train_data)$data
  }

  # Check if dummy vars need to be applied 
  if (!is.null(hyperparameters[["dummy_model"]])) {
    print("Training xgboost - use dummyVars")
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
    ntree = 1000,
    mtry = 'DEFAULT',
    nodesize = 1,
    use_rose = FALSE,
    use_weights = FALSE,
    dummy_model = NULL,
    hyperparameters = hyperparameters
  )

    # ROSE balancing
  if (hyperparameters[["use_rose"]]) {
    train_data <-ROSE(formula , data = train_data)$data
  }
  
  # Weight balancing. Error: variable lengths differ (found for '(weights)')
  model_weights = NULL
  if (hyperparameters[["use_weights"]]) {
    model_weights <- calc_weights(train_data)$model_weights
  }

    # Check if dummy vars need to be applied 
  if (!is.null(hyperparameters[["dummy_model"]])) {
    train_data <- train_data %>% add_dummy_vars(hyperparameters[["dummy_model"]])
  }
  
  if (hyperparameters[["mtry"]] != 'DEFAULT') {
    randomForest(
      formula = formula,
      data = train_data,
      classwt = model_weights,
      ntree = hyperparameters[["ntree"]],
      mtry = hyperparameters[["mtry"]],
      nodesize = hyperparameters[["nodesize"]]
    )
  } else {
    randomForest(
      formula = formula,
      data = train_data,
      classwt = model_weights,
      ntree = hyperparameters[["ntree"]],
      nodesize = hyperparameters[["nodesize"]]
    )
  }
}
