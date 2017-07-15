#' Lead and lag.
#'
#' Find the "next" or "previous" values in a vector. Useful for comparing values
#' ahead of or behind the current values.
#'
#' @param x a vector of values
#' @param n a postive integer of length 1, giving the number of positions to
#'   lead or lag by
#' @param default value used for non-existant rows. Defaults to `NA`.
#' @param order_by override the default ordering to use another vector
#' @param ... Needed for compatibility with lag generic.
#' @importFrom stats lag
#' @examples
#' lead(1:10, 1)
#' lead(1:10, 2)
#'
#' lag(1:10, 1)
#' lead(1:10, 1)
#'
#' x <- runif(5)
#' cbind(ahead = lead(x), x, behind = lag(x))
#'
#' # Use order_by if data not already ordered
#' df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
#' scrambled <- df[sample(nrow(df)), ]
#'
#' wrong <- mutate(scrambled, prev = lag(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, prev = lag(value, order_by = year))
#' arrange(right, year)
#' @name lead-lag
NULL

#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, default = NA, order_by = NULL, ...) {
  if (!is.null(order_by)) {
    return(with_order(order_by, lead, x, n = n, default = default))
  }

  if (length(n) != 1 || !is.numeric(n) || n < 0) {
    bad_args("n", "must be a nonnegative integer scalar, ",
      "not {type_of(n)} of length {length(n)}"
    )
  }
  if (n == 0) return(x)

  xlen <- length(x)
  n <- pmin(n, xlen)

  out <- c(x[-seq_len(n)], rep(default, n))
  attributes(out) <- attributes(x)
  out
}

#' @export
#' @rdname lead-lag
lag <- function(x, n = 1L, default = NA, order_by = NULL, ...) {
  if (!is.null(order_by)) {
    return(with_order(order_by, lag, x, n = n, default = default))
  }

  if (inherits(x, "ts")) {
    bad_args("x", "must be a vector, not a ts object, do you want `stats::lag()`?")
  }

  if (length(n) != 1 || !is.numeric(n) || n < 0) {
    bad_args("n", "must be a nonnegative integer scalar, ",
      "not {type_of(n)} of length {length(n)}"
    )
  }
  if (n == 0) return(x)

  xlen <- length(x)
  n <- pmin(n, xlen)

  out <- c(rep(default, n), x[seq_len(xlen - n)])
  attributes(out) <- attributes(x)
  out
}
