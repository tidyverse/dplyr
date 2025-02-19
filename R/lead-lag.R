#' Compute lagged or leading values
#'
#' Find the "previous" (`lag()`) or "next" (`lead()`) values in a vector. Useful
#' for comparing values behind of or ahead of the current values.
#'
#' @param x A vector
#' @param n Positive integer of length 1, giving the number of positions to
#'   lag or lead by
#' @param default The value used to pad `x` back to its original size after the
#'   lag or lead has been applied. The default, `NULL`, pads with a missing
#'   value. If supplied, this must be a vector with size 1, which will be cast
#'   to the type of `x`.
#' @param order_by An optional secondary vector that defines the ordering to use
#'   when applying the lag or lead to `x`. If supplied, this must be the same
#'   size as `x`.
#' @param ... Not used.
#'
#' @return
#' A vector with the same type and size as `x`.
#'
#' @name lead-lag
#' @examples
#' lag(1:5)
#' lead(1:5)
#'
#' x <- 1:5
#' tibble(behind = lag(x), x, ahead = lead(x))
#'
#' # If you want to look more rows behind or ahead, use `n`
#' lag(1:5, n = 1)
#' lag(1:5, n = 2)
#'
#' lead(1:5, n = 1)
#' lead(1:5, n = 2)
#'
#' # If you want to define a value to pad with, use `default`
#' lag(1:5)
#' lag(1:5, default = 0)
#'
#' lead(1:5)
#' lead(1:5, default = 6)
#'
#' # If the data are not already ordered, use `order_by`
#' scrambled <- slice_sample(
#'   tibble(year = 2000:2005, value = (0:5) ^ 2),
#'   prop = 1
#' )
#'
#' wrong <- mutate(scrambled, previous_year_value = lag(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, previous_year_value = lag(value, order_by = year))
#' arrange(right, year)
NULL

#' @export
#' @rdname lead-lag
lag <- function(x, n = 1L, default = NULL, order_by = NULL, ...) {
  if (inherits(x, "ts")) {
    abort("`x` must be a vector, not a <ts>, do you want `stats::lag()`?")
  }
  check_dots_empty0(...)

  check_number_whole(n)
  if (n < 0L) {
    abort("`n` must be positive.")
  }

  shift(x, n = n, default = default, order_by = order_by)
}

#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, default = NULL, order_by = NULL, ...) {
  check_dots_empty0(...)

  check_number_whole(n)
  if (n < 0L) {
    abort("`n` must be positive.")
  }

  shift(x, n = -n, default = default, order_by = order_by)
}

shift <- function(
  x,
  ...,
  n = 1L,
  default = NULL,
  order_by = NULL,
  error_call = caller_env()
) {
  check_dots_empty0(...)

  if (!is.null(order_by)) {
    out <- with_order(
      order_by = order_by,
      fun = shift,
      x = x,
      n = n,
      default = default,
      error_call = error_call
    )
    return(out)
  }

  obj_check_vector(x, call = error_call)

  check_number_whole(n)
  n <- vec_cast(n, integer(), call = error_call)

  if (!is.null(default)) {
    vec_check_size(default, size = 1L, call = error_call)

    default <- vec_cast(
      x = default,
      to = x,
      x_arg = "default",
      to_arg = "x",
      call = error_call
    )
  }

  lag <- n >= 0L
  n <- abs(n)

  size <- vec_size(x)

  if (n > size) {
    n <- size
  }

  if (is.null(default)) {
    shift_slice(x, n, size, lag)
  } else {
    shift_c(x, n, size, lag, default)
  }
}

shift_slice <- function(x, n, size, lag) {
  loc_default <- vec_rep(NA_integer_, n)

  if (lag) {
    loc <- seq2(1L, size - n)
    loc <- vec_c(loc_default, loc)
    vec_slice(x, loc)
  } else {
    loc <- seq2(1L + n, size)
    loc <- vec_c(loc, loc_default)
    vec_slice(x, loc)
  }
}

shift_c <- function(x, n, size, lag, default) {
  default <- vec_rep(default, n)

  if (lag) {
    loc <- seq2(1L, size - n)
    x <- vec_slice(x, loc)
    vec_c(default, x, .ptype = x)
  } else {
    loc <- seq2(1L + n, size)
    x <- vec_slice(x, loc)
    vec_c(x, default, .ptype = x)
  }
}
