#' Group input by rows
#'
#' \code{rowwise} is used for the results of \code{\link{do}} when you
#' create list-variables. It is also useful to support arbitrary
#' complex operations that need to be applied to each row.
#'
#' Currently \code{rowwise} grouping only works with data frames. It's
#' main impact is to allow you to work with list-variables in
#' \code{\link{summarise}} and \code{\link{mutate}} without having to
#' use \code{[[1]]}. This makes \code{summarise()} on a rowwise tbl
#' effectively equivalent to plyr's \code{ldply}.
#'
#' @param data Input data frame.
#' @export
#' @examples
#' df <- expand.grid(x = 1:3, y = 3:1)
#' df %>% rowwise() %>% do(i = seq(.$x, .$y))
#' .Last.value %>% summarise(n = length(i))
rowwise <- function(data) {
  stopifnot(is.data.frame(data))

  structure(data, class = c("rowwise_df", "tbl_df", "data.frame"))
}

#' @export
print.rowwise_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("Groups: <by row>\n")
  cat("\n")
  trunc_mat(x, n = n, width = width)
}

#' @export
ungroup.rowwise_df <- function(x) {
  class(x) <- setdiff(class(x), "rowwise_df")
  x
}

#' @export
as.data.frame.rowwise_df <- function(x, row.names, optional, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
group_size.rowwise_df <- function(x) {
  rep.int(1L, nrow(x))
}

#' @export
n_groups.rowwise_df <- function(x) {
  nrow(x)
}

#' @export
group_by_.rowwise_df <- function(.data, ..., .dots, add = FALSE) {
  warning("Grouping rowwise data frame strips rowwise nature", call. = FALSE)
  .data <- ungroup(.data)

  groups <- group_by_prepare(.data, ..., .dots = .dots, add = add)
  grouped_df(groups$data, groups$groups)
}
