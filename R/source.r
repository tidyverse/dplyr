new_source <- function(...) {
  structure(list(...), class = c("source", "ops"))
}

source_vars <- function(x) UseMethod("source_vars")

#' Convert an existing object to a data source.
#'
#' @param x an object to convert
#' @param ... other arguments passed to methods
#' @export
#' @examples
#' as.source(mtcars)
as.source <- function(x, ...) UseMethod("as.source")

#' @S3method as.source source
as.source.source <- function(x, ...) x

is.source <- function(x) inherits(x, "source")
