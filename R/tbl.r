#' Create a table from a data source
#'
#' This is a generic method that dispatches based on the first argument.
#'
#' @param src A data source
#' @param ... Other arguments passed on to the individual methods
#' @export
tbl <- function(src, ...) {
  UseMethod("tbl")
}

#' @rdname tbl
#' @export
is.tbl <- function(x) inherits(x, "tbl")

#' @export
#' @rdname tbl
#' @param x an object to coerce to a \code{tbl}
as.tbl <- function(x, ...) UseMethod("as.tbl")

#' @export
as.tbl.tbl <- function(x, ...) x

#' List variables provided by a tbl.
#'
#' @export
#' @param x A tbl object
tbl_vars <- function(x) UseMethod("tbl_vars")
