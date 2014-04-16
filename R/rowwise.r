#' Group input by rows
#'
#' \code{rowwise} is used for the results of \code{\link{do}} when you
#' create list-variables. It is also useful to support arbitrary
#' complex operations that need to be applied to each row.
#'
#' Currently \code{rowwise} grouping only works with data frames.
#'
#' @param data Input data frame.
#' @export
#' @examples
#' df <- expand.grid(x = 1:3, y = 3:1)
#' # df %>% rowwise() %>% do(seq(.$x, .$y))
rowwise <- function(data) {
  stopifnot(is.data.frame(data))

  structure(data, class = c("rowwise_df", "tbl_df", "data.frame"))
}

#' @export
print.rowwise_df <- function(x, ..., n = NULL) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("Groups: <by row>\n")
  cat("\n")
  trunc_mat(x, n = n)
}
