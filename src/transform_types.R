transform_types <- function(df, categorical_var_names=NULL, intiger_var_names=NULL) {
  if (is.null(categorical_var_names) & is.null(intiger_var_names)) {
    return(df)
  }

  for (i in names(df)) {
    if (i %in% categorical_var_names) {
      df[[i]] <- as.factor(df[[i]])
    } else if (i %in% intiger_var_names) {
      df[[i]] <- as.integer(df[[i]])
    }
  }

  return(df)
}