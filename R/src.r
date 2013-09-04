#' Create a "src" object
#'
#' \code{src} is the standard constructor for srcs and \code{is.src} tests. 
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass. "src" is an abstract base class, so you 
#'   must supply this value. \code{src_} is automatically prepended to the 
#'   class name
#' @param ... fields used by object
#' @param x object to test for "src"-ness.
#' @examples
#' as.tbl(mtcars)
src <- function(subclass, ...) {
  subclass <- paste0("src_", subclass)
  structure(list(...), class = c(subclass, "src"))
}

#' @rdname src
#' @export
is.src <- function(x) inherits(x, "src")

#' @S3method print src
print.src <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
}

#' List all tbls provided by a source.
#'
#' This is a generic method which individual src's will provide methods for.
#' Most methods will not be documented because it's usually pretty obvious what
#' possible results will be.
#'
#' @param x a data src. 
#' @export
src_tbls <- function(x) {
  UseMethod("src_tbls")
}

#' Figure out if two sources are the same (or two tbl have the same source)
#' 
#' @param x,y src or tbls to test
#' @return a logical flag
#' @export
#' @keywords internal
same_src <- function(x, y) {
  UseMethod("same_src")
}
