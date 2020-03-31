#' Do values in a numeric vector fall in specified range?
#'
#' This is a shortcut for `x >= left & x <= right`, implemented
#' efficiently in C++ for local values, and translated to the
#' appropriate SQL for remote tables.
#'
#' @param x A numeric vector of values
#' @param left,right Boundary values
#' @export
#' @examples
#' between(1:12, 7, 9)
#'
#' x <- rnorm(1e2)
#' x[between(x, -1, 1)]
between <- function(x, left, right) {
  if (!is.null(attr(x, "class")) && !inherits(x, c("Date", "POSIXct"))) {
    warn("between() called on numeric vector with S3 class");
  }

  if (!is.double(x)) {
    x <- as.numeric(x)
  }
  .Call(`dplyr_between`, x, as.numeric(left), as.numeric(right))
}

#' Cumulativate versions of any, all, and mean
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
#' df %>% filter(cumany(balance < 0))
#' # all rows until first overdraft
#' df %>% filter(cumall(!(balance < 0)))
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
