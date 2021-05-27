cross_validate <- function(train_data, model, predictor, formulas, num_fold_cols=20) {
  cross_validate_fn(
    data = train_data,
    formulas = formulas,
    type = "binomial",
    model_fn = model,
    predict_fn = predictor,
    fold_cols = paste0(".folds_", 1:num_fold_cols),
    parallel = TRUE
  )
}