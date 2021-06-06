create_normalize_data_model <- function(df, columns=c("training_hours")) {
  result = list()

  for (col in columns) {
    mean <- mean(df[[col]])
    sd <- sd(df[[col]])
    result[[col]] <- list(mean=mean, sd=sd)
  }

  return(result)
}

normalize_data <- function(df, model=list(training_hours=list(mean=0, std=1))) {
  for (col in names(model)) {
    df[col] <- (df[col] - model[[col]]$mean) / model[[col]]$sd
  }

  return(df)
}