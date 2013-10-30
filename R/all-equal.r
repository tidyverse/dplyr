#' Provide a useful implementation of all.equal for data.frames.
#' 
#' @param target,current two data frames to compare
#' @param ignore_col_order should order of columns be ignored?
#' @param ignore_row_order should order of rows be ignored?
#' @param convert Should similar classes be converted? Currently this will
#'   convert factor to character and integer to double.
#' @return \code{TRUE} if equal, otherwise a character vector describing
#'   the first reason why they're not equal. Use \code{\link{isTRUE}} if
#'   using the result in an \code{if} expression.
#' @method all.equal data.frame
#' @export
#' @examples
#' scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
#' 
#' # By default, ordering of rows and columns ignored
#' all.equal(mtcars, scramble(mtcars))
#' 
#' # But those can be overriden if desired
#' all.equal(mtcars, scramble(mtcars), ignore_col_order = FALSE)
#' all.equal(mtcars, scramble(mtcars), ignore_row_order = FALSE)
all.equal.data.frame <- function(target, current, ignore_col_order = TRUE, 
                                 ignore_row_order = TRUE, convert = FALSE) {
  
  res <- equal_data_frame(target, current, ignore_col_order = ignore_col_order,
    ignore_row_order = ignore_row_order, convert = convert)
  
  if (res) {
    TRUE
  } else {
    attr(res, "comment")
  }
}
