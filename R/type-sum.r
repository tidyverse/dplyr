#' Provide a succint summary of a type
#'
#' All methods should return a string with four or less characters, suitable
#' for succinctly display column types.
#'
#' @param x an object to summarise. Generally only methods of atomic vectors
#'   and variants have been implemented.
#' @keywords internal
#' @export
#' @examples
#' type_sum(1:10)
#' type_sum(matrix(1:10))
#' type_sum(Sys.Date())
#' type_sum(Sys.time())
#' type_sum(mean)
type_sum <- function(x) UseMethod("type_sum")

#' @export
type_sum.data.frame <- function(x) {
  vapply(x, type_sum, character(1))
}

#' @export
type_sum.numeric <- function(x) "dbl"
#' @export
type_sum.integer <- function(x) "int"
#' @export
type_sum.logical <- function(x) "lgl"
#' @export
type_sum.character <- function(x) "chr"

#' @export
type_sum.factor <- function(x) "fctr"
#' @export
type_sum.POSIXt <- function(x) "time"
#' @export
type_sum.Date <- function(x) "date"

#' @export
type_sum.matrix <- function(x) {
  paste0(NextMethod(), "[", paste0(dim(x), collapse = ","), "]")
}
#' @export
type_sum.array <- type_sum.matrix

#' @export
type_sum.default <- function(x) unname(abbreviate(class(x)[1], 4))
