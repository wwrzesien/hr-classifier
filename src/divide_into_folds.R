divide_into_folds <- function(df, k=10, num_fold_cols=20, cat_col='target', id_col=NULL) {
  groupdata2::fold(df,
        k = k,
        cat_col = cat_col,
        id_col = id_col,
        num_fold_cols = num_fold_cols,
        parallel = TRUE)
}