#' Extract the first, last, or nth value from a vector
#'
#' These are useful helpers for extracting a single value from a vector. They
#' always return a result that is size 1, even when the input is shorter than
#' expected. You can also provide an optional secondary vector that defines the
#' ordering.
#'
#' @param x A vector
#' @param n For `nth()`, a single integer specifying the position.
#'   Negative integers index from the end (i.e. `-1L` will return the
#'   last value in the vector).
#' @param order_by An optional vector the same size as `x` used to determine the
#'   order.
#' @param default A default value to use if the position does not exist in `x`.
#'
#'   If `NULL`, the default, a missing value is used.
#'
#'   If supplied, this must be a single value, which will be cast to the type of
#'   `x`.
#'
#' @return
#' A vector the same type as `x` with size 1.
#'
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
#'
#' # `first()` and `last()` are often useful in `summarise()`
#' df <- tibble(x = x, y = y)
#' df %>%
#'   summarise(
#'     across(x:y, first, .names = "{col}_first"),
#'     y_last = last(y)
#'   )
#'
#' # Selecting a position that is out of bounds returns a default value
#' nth(x, 11)
#' nth(x, 0)
#'
#' last(x)
#' # `order_by` provides optional ordering
#' last(x, order_by = y)
#'
#' # These functions always return a single value, even with empty vectors
#' first(integer())
#'
#' # For data frames, these select entire rows
#' df <- tibble(a = 1:5, b = 6:10)
#' first(df)
#' nth(df, 4)
nth <- function(x, n, order_by = NULL, default = NULL) {
  size <- vec_size(x)

  vec_assert(n, size = 1L, arg = "n")
  n <- vec_cast(n, to = integer(), x_arg = "n")

  if (!is.null(order_by)) {
    vec_assert(order_by, size = size, arg = "order_by")
  }

  if (is.null(default)) {
    default <- vec_init(x)
  } else {
    vec_assert(default, size = 1L, arg = "default")
    default <- vec_cast(default, to = x, x_arg = "default", to_arg = "x")
  }

  if (n < 0L) {
    # Negative values index from RHS
    n <- size + n + 1L
  }

  if (n <= 0L || n > size) {
    return(default)
  }

  if (!is.null(order_by)) {
    order <- vec_order_base(order_by)
    n <- order[[n]]
  }

  vec_slice(x, n)
}

#' @export
#' @rdname nth
first <- function(x, order_by = NULL, default = NULL) {
  nth(x, 1L, order_by = order_by, default = default)
}

#' @export
#' @rdname nth
last <- function(x, order_by = NULL, default = NULL) {
  nth(x, -1L, order_by = order_by, default = default)
}
