#' Join two tbls together.
#'
#' These are generic functions that dispatch to individual tbl methods - see the
#' method documentation for details of individual data sources. \code{x} and
#' \code{y} should usually be from the same data source, but if \code{copy} is
#' \code{TRUE}, \code{y} will automatically be copied to the same source as
#' \code{x} - this may be an expensive operation.
#'
#' @section Join types:
#'
#' Currently dplyr supports four join types:
#'
#' \itemize{
#'    \code{inner_join}: return all rows from x where there are matching values
#'    in y, and all columns from x and y. If there are multiple matches between
#'    x and y, all combination of the matches are returned.
#'
#'    \code{left_join}: return all rows from x, and all columns from x and y.
#'    If there are multiple matches between x and y, all combination of the
#'    matches are returned.
#'
#'    \code{semi_join}: return all rows from x where there are matching values
#'    in y, keeping just columns from x.
#'
#'    \code{anti_join}: return all rows from x where there are not matching
#'    values in y, keeping just columns from x
#' }
#'
#' A semi join differs from an inner join (with just the \code{by} variables)
#' because an inner join will return one row of \code{x} for each matching row
#' of \code{y}, where a semi join will never duplicated rows in \code{x}.
#'
#' @section Grouping:
#'
#' Groups are ignored for the purpose of joining, but the result preserves
#' the grouping of \code{x}.
#'
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If \code{NULL}, the
#'   default, \code{join} will do a natural join, using all variables with
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right.
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
#'   same src as \code{x}.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param ... other parameters passed onto methods
#' @name join
NULL

#' @rdname join
#' @export
inner_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("inner_join")
}

#' @rdname join
#' @export
left_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("left_join")
}

#' @rdname join
#' @export
semi_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("semi_join")
}

#' @rdname join
#' @export
anti_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("anti_join")
}

common_by <- function(x, y) {
  by <- intersect(tbl_vars(x), tbl_vars(y))
  message("Joining by: ", capture.output(dput(by)))
  by
}

unique_names <- function(x_names, y_names, by, x_suffix = ".x", y_suffix = ".y") {
  common <- setdiff(intersect(x_names, y_names), by)
  if (length(common) == 0) return(NULL)

  x_match <- match(common, x_names)
  x_names[x_match] <- paste0(x_names[x_match], x_suffix)

  y_match <- match(common, y_names)
  y_names[y_match] <- paste0(y_names[y_match], y_suffix)

  list(x = x_names, y = y_names)
}
