cross_validate <- function(model, predictor, formulas) {
  cross_validate <- cross_validate_fn(
    data = data,
    formulas = formulas,
    type = "gaussian",
    model_fn = model,
    predict_fn = predictor,
    fold_cols = paste0(".folds_", 1:5),
    parallel = TRUE
  )
}