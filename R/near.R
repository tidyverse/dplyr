#' Compare two numeric vectors
#'
#' This is a safe way of comparing if two vectors of floating point numbers
#' are (pairwise) equal.  This is safer than using `==`, because it has
#' a built in tolerance
#'
#' @param x,y Numeric vectors to compare
#' @param tol Tolerance of comparison.
#' @export
#' @examples
#' sqrt(2) ^ 2 == 2
#' near(sqrt(2) ^ 2, 2)
near <- function(x, y, tol = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tol
}
