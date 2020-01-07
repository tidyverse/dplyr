#' Extract the first, last or nth value from a vector
#'
#' These are straightforward wrappers around \code{\link{[[}}. The main
#' advantage is that you can provide an optional secondary vector that defines
#' the ordering, and provide a default value to use when the input is shorter
#' than expected.
#'
#' @param x A vector
#' @param n For `nth_value()`, a single integer specifying the position.
#'   Negative integers index from the end (i.e. `-1L` will return the
#'   last value in the vector).
#'
#'   If a double is supplied, it will be silently truncated.
#' @param order_by An optional vector used to determine the order
#' @param default A default value to use if the position does not exist in
#'   the input. This is guessed by default for base vectors, where a
#'   missing value of the appropriate type is returned, and for lists, where
#'   a `NULL` is return.
#'
#'   For more complicated objects, you'll need to supply this value.
#'   Make sure it is the same type as `x`.
#' @return A single value. `[[` is used to do the subsetting.
#' @export
#' @examples
#' x <- 1:10
#' y <- 10:1
#'
#' first(x)
#' last(y)
#'
#' nth(x, 1)
#' nth(x, 5)
#' nth(x, -2)
#' nth(x, 11)
#'
#' last(x)
#' # Second argument provides optional ordering
#' last(x, y)
#'
#' # These functions always return a single value
#' first(integer())
nth <- function(x, n, order_by = NULL, default = default_missing(x)) {
  abort_if_not(length(n) == 1, is.numeric(n))
  n <- trunc(n)

  if (n == 0 || n > length(x) || n < -length(x)) {
    return(default)
  }

  # Negative values index from RHS
  if (n < 0) {
    n <- length(x) + n + 1
  }

  if (is.null(order_by)) {
    x[[n]]
  } else {
    x[[ order(order_by)[[n]] ]]
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
  nth(x, -1L, order_by = order_by, default = default)
}

default_missing <- function(x) {
  UseMethod("default_missing")
}

#' @export
default_missing.default <- function(x) {
  if (!is.object(x) && is.list(x)) {
    NULL
  } else {
    x[NA_real_]
  }
}

#' @export
default_missing.data.frame <- function(x) {
  rep(NA, nrow(x))
}
