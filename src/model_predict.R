model_predict <- function(test_data, model, formula, hyperparameters, train_data) {
  predict(object = model,
          newdata = test_data,
          allow.new.levels = TRUE)
}