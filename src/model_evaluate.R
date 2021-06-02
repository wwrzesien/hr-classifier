model_evaluate <- function(test_set, target_col = "target", prediction_cols = "predicted_class") {
   evaluate(data = test_set, target_col = target_col, prediction_cols = prediction_cols, type = "binomial")
}