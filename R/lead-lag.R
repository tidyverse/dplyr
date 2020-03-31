#' Compute lagged or leading values
#'
#' Find the "previous" (`lag()`) or "next" (`lead()`) values in a vector.
#' Useful for comparing values behind of or ahead of the current values.
#'
#' @param x Vector of values
#' @param n Positive integer of length 1, giving the number of positions to
#'   lead or lag by
#' @param default Value used for non-existent rows. Defaults to `NA`.
#' @param order_by Override the default ordering to use another vector or column
#' @param ... Needed for compatibility with lag generic.
#' @importFrom stats lag
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
#' # If you want to define a value for non-existing rows, use `default`
#' lag(1:5)
#' lag(1:5, default = 0)
#'
#' lead(1:5)
#' lead(1:5, default = 6)
#'
#' # If data are not already ordered, use `order_by`
#' scrambled <- slice_sample(tibble(year = 2000:2005, value = (0:5) ^ 2), prop = 1)
#'
#' wrong <- mutate(scrambled, previous_year_value = lag(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, previous_year_value = lag(value, order_by = year))
#' arrange(right, year)
#' @name lead-lag
NULL

leadlag_default <- function(x, default) {
  if (!identical(default, NA)) {
    default <- tryCatch(
      vec_cast(default, x),
      vctrs_error_incompatible_cast = function(cnd) {
        abort(c(
          glue("Incompatible type for `default`."),
          i = glue("<{vec_ptype_full(default)}> cannot be cast to type <{vec_ptype_full(x)}>.")
        ))
      }
    )
  }
  default
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
      "not {friendly_type_of(n)} of length {length(n)}"
    )
  }
  if (n == 0) return(x)

  xlen <- vec_size(x)
  n <- pmin(n, xlen)

  default <- leadlag_default(x, default)
  vec_c(
    vec_rep(default, n),
    vec_slice(x, seq_len(xlen - n))
  )
}

#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, default = NA, order_by = NULL, ...) {
  if (!is.null(order_by)) {
    return(with_order(order_by, lead, x, n = n, default = default))
  }

  if (length(n) != 1 || !is.numeric(n) || n < 0) {
    bad_args("n", "must be a nonnegative integer scalar, ",
             "not {friendly_type_of(n)} of length {length(n)}"
    )
  }
  if (n == 0) return(x)

  xlen <- vec_size(x)
  n <- pmin(n, xlen)

  default <- leadlag_default(x, default)
  vec_c(
    vec_slice(x, -seq_len(n)),
    vec_rep(default, n)
  )
}
