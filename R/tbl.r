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
tbl <- function(subclass, ...) {
  subclass <- paste0("tbl_", subclass)
  structure(list(...), class = c(subclass, "tbl", "ops"))
}

#' @rdname tbl
#' @export
is.tbl <- function(x) inherits(x, "tbl")

#' @export
#' @rdname tbl
as.tbl <- function(x, ...) UseMethod("as.tbl")

#' @S3method as.tbl tbl
as.tbl.tbl <- function(x, ...) x

#' List variables provided by a tbl.
#'
#' @export
#' @param x A tbl object
tbl_vars <- function(x) UseMethod("tbl_vars")
