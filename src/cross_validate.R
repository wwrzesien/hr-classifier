cross_validate <- function(train_data, formulas, model_fn, predict_fn, hyperparameters, fold_cols) {
  cross_validate_fn(
    data = train_data,
    formulas = formulas,
    model_fn = model,
    predict_fn = predict_fn,
    hyperparameters = hyperparameters,
    fold_cols = fold_cols,
    type = "binomial",
    parallel = TRUE
  )
}