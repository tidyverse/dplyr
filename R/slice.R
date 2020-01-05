#' Choose rows by position
#'
#' Choose rows by their ordinal position in the tbl.  Grouped tbls use
#' the ordinal position within the group.
#'
#' Slice does not work with relational databases because they have no
#' intrinsic notion of row order. If you want to perform the equivalent
#' operation, use [filter()] and [row_number()].
#'
#' @family single table verbs
#' @param .data A tbl.
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Integer row values.
#'   Provide either positive values to keep, or negative values to drop.
#'   The values provided must be either all positive or all negative.
#'   Indices beyond the number of rows in the input are silently ignored.
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @export
#' @examples
#' slice(mtcars, 1L)
#' # Similar to tail(mtcars, 1):
#' slice(mtcars, n())
#' slice(mtcars, 5:n())
#' # Rows can be dropped with negative indices:
#' slice(mtcars, -5:-n())
#' # In this case, the result will be equivalent to:
#' slice(mtcars, 1:4)
#'
#' by_cyl <- group_by(mtcars, cyl)
#' slice(by_cyl, 1:2)
#'
#' # Equivalent code using filter that will also work with databases,
#' # but won't be as fast for in-memory data. For many databases, you'll
#' # need to supply an explicit variable to use to compute the row number.
#' filter(mtcars, row_number() == 1L)
#' filter(mtcars, row_number() == n())
#' filter(mtcars, between(row_number(), 5, n()))
slice <- function(.data, ..., .preserve = FALSE) {
  UseMethod("slice")
}
