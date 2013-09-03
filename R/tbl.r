#' Create a new datasource.
#'
#' Standard constructor for tbls.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... other fields used by class
tbl <- function(subclass, ...) {
  structure(list(...), class = c(subclass, "tbl", "ops"))
}

#' List variables provided by a tbl.
#'
#' @export
#' @param x A tbl object
tbl_vars <- function(x) UseMethod("tbl_vars")

#' Convert an existing object to a tbl.
#'
#' @param x an object to convert
#' @param ... other arguments passed to methods
#' @export
#' @examples
#' as.tbl(mtcars)
as.tbl <- function(x, ...) UseMethod("as.tbl")

#' @S3method as.tbl tbl
as.tbl.tbl <- function(x, ...) x

#' Is this object a tbl?
#'
#' @param x an object
#' @keywords internal
#' @export
is.tbl <- function(x) inherits(x, "tbl")
