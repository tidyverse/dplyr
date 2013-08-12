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

#' @S3method type_sum numeric
type_sum.numeric <- function(x) "dbl"
#' @S3method type_sum integer
type_sum.integer <- function(x) "int"
#' @S3method type_sum logical
type_sum.logical <- function(x) "lgl"
#' @S3method type_sum character
type_sum.character <- function(x) "chr"

#' @S3method type_sum factor
type_sum.factor <- function(x) "fctr"
#' @S3method type_sum POSIXt
type_sum.POSIXt <- function(x) "time"
#' @S3method type_sum Date
type_sum.Date <- function(x) "date"

#' @S3method type_sum matrix
type_sum.matrix <- function(x) {
  paste0(NextMethod(), "[", paste0(dim(x), collapse = ","), "]")
}
#' @S3method type_sum array
type_sum.array <- type_sum.matrix

#' @S3method type_sum default
type_sum.default <- function(x) unname(abbreviate(class(x)[1], 4))
