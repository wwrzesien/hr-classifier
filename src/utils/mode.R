# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
mode <- function(df, na.rm = FALSE) {
  result = df
  if(na.rm) {
    result = result[!is.na(result)]
  }

  result <- unique(result)
  return(result[which.max(tabulate(match(df, result)))])
}