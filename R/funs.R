#' Detect where values fall in a specified range
#'
#' This is a shortcut for `x >= left & x <= right`, implemented for local
#' vectors and translated to the appropriate SQL for remote tables.
#'
#' @details
#' `x`, `left`, and `right` are all cast to their common type before the
#' comparison is made. Use the `ptype` argument to specify the type manually.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x A vector
#' @param left,right Boundary values. Both `left` and `right` are recycled to
#'   the size of `x`.
#' @param ptype An optional prototype giving the desired output type. The
#'   default is to compute the common type of `x`, `left`, and `right` using
#'   [vctrs::vec_cast_common()].
#'
#' @returns
#' A logical vector the same size as `x` with a type determined by `ptype`.
#'
#' @seealso
#' [join_by()] if you are looking for documentation for the `between()` overlap
#' join helper.
#'
#' @export
#' @examples
#' between(1:12, 7, 9)
#'
#' x <- rnorm(1e2)
#' x[between(x, -1, 1)]
#'
#' # On a tibble using `filter()`
#' filter(starwars, between(height, 100, 150))
#'
#' # Using the `ptype` argument with ordered factors, where otherwise everything
#' # is cast to the common type of character before the comparison
#' x <- ordered(
#'   c("low", "medium", "high", "medium"),
#'   levels = c("low", "medium", "high")
#' )
#' between(x, "medium", "high")
#' between(x, "medium", "high", ptype = x)
between <- function(x, left, right, ..., ptype = NULL) {
  check_dots_empty0(...)

  args <- list(x = x, left = left, right = right)

  # Common type of all inputs
  args <- vec_cast_common(!!!args, .to = ptype)
  x <- args$x
  args$x <- NULL

  # Recycle to size of `x`
  args <- vec_recycle_common(!!!args, .size = vec_size(x))
  left <- args$left
  right <- args$right

  left <- vec_compare(x, left)
  left <- left >= 0L

  right <- vec_compare(x, right)
  right <- right <= 0L

  left & right
}

#' Cumulative versions of any, all, and mean
#'
#' dplyr provides `cumall()`, `cumany()`, and `cummean()` to complete R's set
#' of cumulative functions.
#'
#' @section Cumulative logical functions:
#'
#' These are particularly useful in conjunction with `filter()`:
#'
#' * `cumall(x)`: all cases until the first `FALSE`.
#' * `cumall(!x)`: all cases until the first `TRUE`.
#' * `cumany(x)`: all cases after the first `TRUE`.
#' * `cumany(!x)`: all cases after the first `FALSE`.
#'
#' @param x For `cumall()` and `cumany()`, a logical vector; for
#'   `cummean()` an integer or numeric vector.
#' @return A vector the same length as `x`.
#' @examples
#' # `cummean()` returns a numeric/integer vector of the same length
#' # as the input vector.
#' x <- c(1, 3, 5, 2, 2)
#' cummean(x)
#' cumsum(x) / seq_along(x)
#'
#' # `cumall()` and `cumany()` return logicals
#' cumall(x < 5)
#' cumany(x == 3)
#'
#' # `cumall()` vs. `cumany()`
#' df <- data.frame(
#'   date = as.Date("2020-01-01") + 0:6,
#'   balance = c(100, 50, 25, -25, -50, 30, 120)
#' )
#' # all rows after first overdraft
#' df |> filter(cumany(balance < 0))
#' # all rows until first overdraft
#' df |> filter(cumall(!(balance < 0)))
#'
#' @export
cumall <- function(x) {
  .Call(`dplyr_cumall`, as.logical(x))
}

#' @rdname cumall
#' @export
cumany <- function(x) {
  .Call(`dplyr_cumany`, as.logical(x))
}

#' @rdname cumall
#' @export
cummean <- function(x) {
  .Call(`dplyr_cummean`, as.numeric(x))
}
