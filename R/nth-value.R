#' Extract the first, last or nth value from a vector.
#' 
#' These are straightforward wrappers around \code{\link{[[}}. The main 
#' advantage is that you can provide an optional secondary vector that defines
#' the ordering, and provide a default value to use when the input is shorter
#' than expected.
#' 
#' @param x A vector
#' @param n For \code{nth_value}, a single integer specifying the position. 
#'   If a numeric is supplied, it will be silently truncated.
#' @param order_by An optional vector used to determine the order
#' @param default A default value to use if the position does not exist in
#'   the input. This is guessed by default for atomic vectors, where a
#'   missing value of the appropriate type is return, and for lists, where
#'   a \code{NULL} is return. For more complicated objects, you'll need to
#'   supply this value.
#' @return A single value. \code{[[} is used to do the subsetting.
#' @export
#' @examples
#' x <- 1:10
#' y <- 10:1
#' 
#' last(x)
#' last(x, y)
nth <- function(x, n, order_by = NULL, default = default_missing(x)) {
  stopifnot(length(n) == 1, is.numeric(n), n >= 0)
  n <- trunc(n)
  
  if (n == 0 || n > length(x)) {
    return(default)
  }
  
  if (is.null(order_by)) {
    x[[n]] 
  } else {
    x[[order(order_by)[n]]]
  }
}

#' @export
#' @rdname nth
first <- function(x, order_by = NULL, default = default_missing(x)) {
  nth(x, 1L, order_by = order_by, default = default)
}

#' @export
#' @rdname nth
last <- function(x, order_by = NULL, default = default_missing(x)) {
  nth(x, length(x), order_by = order_by, default = default)
}

default_missing <- function(x) {
  # The user needs to supply a default for anything with attributes 
  if (!is.vector(x)) {
    stop("Don't know how to generate default for object of class ", 
      paste0(class(x), collapse = "/"), call. = FALSE)    
  }

  if (is.list(x)) {
    NULL
  } else if (is.vector(x) && is.atomic(x)) {
    def <- NA
    storage.mode(def) <- storage.mode(x)
    def
  }
}


