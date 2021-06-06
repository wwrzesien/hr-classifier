destruct_formula <- function(formula) {
  # Only applicable to formulas with independent vars connected with "+"
  formula_split <- lapply(as.character(formula), stringr::str_squish)
  dependent <- formula_split[2]
  independent <- stringr::str_split(formula_split[3], "\\+")
  independent <- lapply(independent, stringr::str_squish)

  return(list(dependent = dependent[[1]], independent = independent[[1]]))
}