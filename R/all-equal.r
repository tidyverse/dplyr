#' Flexible equality comparison for data frames.
#'
#' You can use \code{all_equal} with any data frame, and dplyr also provides
#' \code{tbl_df} methods for \code{\link{all.equal}}.
#'
#' @param target,current Two data frames to compare.
#' @param ignore_col_order Should order of columns be ignored?
#' @param ignore_row_order Should order of rows be ignored?
#' @param convert Should similar classes be converted? Currently this will
#'   convert factor to character and integer to double.
#' @param ... Ignored. Needed for compatibility with \code{all.equal}.
#' @return \code{TRUE} if equal, otherwise a character vector describing
#'   the reasons why they're not equal. Use \code{\link{isTRUE}} if using the
#'   result in an \code{if} expression.
#' @export
#' @examples
#' scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
#'
#' # By default, ordering of rows and columns ignored
#' all_equal(mtcars, scramble(mtcars))
#'
#' # But those can be overriden if desired
#' all_equal(mtcars, scramble(mtcars), ignore_col_order = FALSE)
#' all_equal(mtcars, scramble(mtcars), ignore_row_order = FALSE)
#'
#' # By default all_equal is sensitive to variable differences
#' df1 <- data.frame(x = "a")
#' df2 <- data.frame(x = factor("a"))
#' all_equal(df1, df2)
#' # But you can request dplyr convert similar types
#' all_equal(df1, df2, convert = TRUE)
all_equal <- function(target, current, ignore_col_order = TRUE,
                      ignore_row_order = TRUE, convert = FALSE, ...) {

  res <- equal_data_frame(target, current, ignore_col_order = ignore_col_order,
    ignore_row_order = ignore_row_order, convert = convert)

  if (res) {
    TRUE
  } else {
    attr(res, "comment")
  }
}

#' @export
#' @rdname all_equal
#' @method all.equal tbl_df
all.equal.tbl_df <- all_equal
