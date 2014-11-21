#' Efficiently bind multiple data frames by row and column.
#'
#' This is an efficient version of the common pattern of
#' \code{do.call(rbind, dfs)} or \code{do.call{cbind, dfs}} for binding many
#' data frames into one.
#'
#' It works in the same way as \code{\link[plyr]{rbind.fill}} but is
#' implemented in C++ so avoids many copies and is much much faster.
#'
#' @param dots,... list of data frames to combine. With \code{*_all},
#'   they should already be in a list, with \code{*_list} you supply
#'   them individually.
#'
#'   When column-binding, rows are matched by position, not value so all data
#'   frames must have the same number of rows. To match by value, not
#'   position, see \code{left_join} etc. When row-binding, columns are
#'   matched by name, and any values that don't match will be filled with NA.
#' @examples
#' one <- mtcars[1:10, ]
#' two <- mtcars[11:20, ]
#'
#' rbind_list(one, two)
#' rbind_all(list(one, two))
#'
#' # Columns don't need to match when row-binding
#' rbind_list(data.frame(x = 1:3), data.frame(y = 1:4))
#' \dontrun{
#' # Rows do need to match when column-binding
#' cbind_list(data.frame(x = 1), data.frame(y = 1:2))
#' }
#'
#' cbind_list(one, two)
#' cbind_all(list(one, two))
#' @name bind
NULL

#' @export
#' @rdname bind
rbind_list <- function(...){
  rbind_list__impl(environment())
}

#' @export
#' @rdname bind
cbind_list <- function(...){
  cbind_list__impl(environment())
}
