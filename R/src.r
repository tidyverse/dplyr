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