#' Provide a useful implementation of all.equal for data.frames.
#'
#' @param target,current two data frames to compare
#' @param ignore_col_order should order of columns be ignored?
#' @param ignore_row_order should order of rows be ignored?
#' @param convert Should similar classes be converted? Currently this will
#'   convert factor to character and integer to double.
#' @param ... Ignored. Needed for compatibility with the generic.
#' @return \code{TRUE} if equal, otherwise a character vector describing
#'   the first reason why they're not equal. Use \code{\link{isTRUE}} if
#'   using the result in an \code{if} expression.
#' @method all.equal tbl_df
#' @export
#' @examples
#' scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
#'
#' # By default, ordering of rows and columns ignored
#' mtcars_df <- tbl_df(mtcars)
#' all.equal(mtcars_df, scramble(mtcars_df))
#'
#' # But those can be overriden if desired
#' all.equal(mtcars_df, scramble(mtcars_df), ignore_col_order = FALSE)
#' all.equal(mtcars_df, scramble(mtcars_df), ignore_row_order = FALSE)
all.equal.tbl_df <- function(target, current, ignore_col_order = TRUE,
                                 ignore_row_order = TRUE, convert = FALSE, ...) {

  res <- equal_data_frame(target, current, ignore_col_order = ignore_col_order,
    ignore_row_order = ignore_row_order, convert = convert)

  if (res) {
    TRUE
  } else {
    attr(res, "comment")
  }
}

#' @method all.equal tbl_dt
#' @export
#' @rdname all.equal.tbl_df
all.equal.tbl_dt <- all.equal.tbl_df
