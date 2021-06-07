calc_weights <- function(df) {
  df_nrows = nrow(df)
  df_0_nrows = sum(df$target == "0")
  df_1_nrows = df_nrows - df_0_nrows
  weight_0 = df_nrows / (df_0_nrows * 2)
  weight_1 = df_nrows / (df_1_nrows * 2)

  model_weights <- ifelse(df$target == "0", weight_0, weight_1)

  return(list(model_weights=model_weights, weight_0=weight_0, weight_1=weight_1))
}