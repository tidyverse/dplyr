#' Extract the first, last or nth value from a vector.
#' 
#' These are straightforward wrappers around \code{\link{[}}. The main 
#' advantage is that you can provide an optional secondary vector that defines
#' the ordering.
#' 
#' @param x A vector
#' @param n For \code{nth_value}, a single integer specifying the position.
#'   If larger than x, an \code{NA} of the same type as x will be returned.
#' @param order_by An optional vector used to determine the order
#' @export
#' @examples
#' x <- 1:10
#' y <- 10:1
#' 
#' last(x)
#' last(x, y)
nth <- function(x, n, order_by = NULL) {
  stopifnot(length(n) == 1, is.numeric(x))
  
  # if n > length(x), x[n] will be NA of correct type
  if (is.null(order_by)) x[n] else x[order(order_by)[n]]
}

#' @export
#' @rdname nth
first <- function(x, order_by = NULL) {
  nth(x, 1, order_by)
}

#' @export
#' @rdname nth
last <- function(x, order_by = NULL) {
  nth(x, length(x), order_by)
}

