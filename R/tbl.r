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

#' Create a "tbl" object
#'
#' \code{tbl} is the standard constructor for tbls. \code{as.tbl} coerces,
#' and \code{is.tbl} tests. 
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass. "tbl" is an abstract base class, so you 
#'   must supply this value. \code{tbl_} is automatically prepended to the 
#'   class name
#' @param object to test/coerce.
#' @param ... For \code{tbl}, other fields used by class. For \code{as.tbl},
#'   other arguments passed to methods.
#' @examples
#' as.tbl(mtcars)
make_tbl <- function(subclass, ...) {
  subclass <- paste0("tbl_", subclass)
  structure(list(...), class = c(subclass, "tbl"))
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
