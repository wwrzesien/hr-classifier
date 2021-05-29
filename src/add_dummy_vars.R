add_dummy_vars <- function(df, dummy_model) {
   return(data.frame(predict(dummy_model, newdata=df)))
}