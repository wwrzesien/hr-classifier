create_model_formula <- function(dependent_var, independent_var_vec) {
   return(as.formula(paste(c(dependent_var, paste(independent_var_vec, collapse=' + ')), collapse=' ~ ')))
}