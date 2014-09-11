#' Join data frame tbls.
#'
#' See \code{\link{join}} for a description of the general purpose of the
#' functions.
#'
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If \code{NULL}, the
#'   default, \code{join} will do a natural join, using all variables with
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right - to suppress the message, supply
#'   a character vector.
#' @param copy If \code{y} is not a data frame or \code{\link{tbl_df}} and
#'   \code{copy} is \code{TRUE}, \code{y} will be converted into a data frame
#' @param ... included for compatibility with the generic; otherwise ignored.
#' @examples
#' if (require("Lahman")) {
#' batting_df <- tbl_df(Batting)
#' person_df <- tbl_df(Master)
#'
#' uperson_df <- tbl_df(Master[!duplicated(Master$playerID), ])
#'
#' # Inner join: match batting and person data
#' inner_join(batting_df, person_df)
#' inner_join(batting_df, uperson_df)
#'
#' # Left join: match, but preserve batting data
#' left_join(batting_df, uperson_df)
#'
#' # Anti join: find batters without person data
#' anti_join(batting_df, person_df)
#' # or people who didn't bat
#' anti_join(person_df, batting_df)
#' }
#' @name join.tbl_df
NULL

#' @export
#' @rdname join.tbl_df
inner_join.tbl_df <- function(x, y, by_x = NULL, by_y = by_x, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  inner_join_impl(x, y, by)
}

#' @export
#' @rdname join.tbl_df
left_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  left_join_impl(x, y, by)
}

#' @export
#' @rdname join.tbl_df
semi_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  semi_join_impl(x, y, by)
}

#' @export
#' @rdname join.tbl_df
anti_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  anti_join_impl(x, y, by)
}
