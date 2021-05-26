transform_types <- function(df, categorical_var_names) {
  for (i in names(df)) {
    if (i %in% categorical_vars_names) {
       df[[i]] <- as.factor(df[[i]])
    }
  }

  return(df)
}