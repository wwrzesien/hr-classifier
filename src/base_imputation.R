# create_mean_median_imputation_model
#
# Create model base model for imputation. Base means it handles mean, median or mode.
# The purpose of a mode is to compute needed statistics which will be applied by base_imputation
# funciton to perform final imputation of dataset (train or test).
#
# @param df - dataframe
# @param columns - list of columns with specified type of imptation
#                  eg. list(gender='mode', time_spent='mean') 
create_base_imputation_model <- function(df, columns=NULL) {
  result = list()
  for (col in names(columns)) {
    if (col %in% names(df)) {
      if (columns[[col]] == 'mean') {
        result[[col]] <- list(mean=mean(df[[col]]))
      } else  if (columns[[col]] == 'median') {
        result[[col]] <- list(mean=median(df[[col]]))
      } else if (columns[[col]] == 'mode') {
        result[[col]] <- list(mean=mode(df[[col]], TRUE))
      } else {
        stop(paste(c('Unsupported type of imputation in create_base_imputation_model.', 
                      'Provided type:', as.character(columns[[col]]))))
      }
    }
  }
  return(result)
}

base_imputation <- function(df, base_imputation_model) {
  for (col in names(base_imputation_model)) {
    value <- base_imputation_model[[col]][1]
    df[is.na(df[, col]), col] <- value
  }

  return(df)
}