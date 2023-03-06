#' Extract the first, last, or nth value from a vector
#'
#' These are useful helpers for extracting a single value from a vector. They
#' are guaranteed to return a meaningful value, even when the input is shorter
#' than expected. You can also provide an optional secondary vector that defines
#' the ordering.
#'
#' @details
#' For most vector types, `first(x)`, `last(x)`, and `nth(x, n)` work like
#' `x[[1]]`, `x[[length(x)]`, and `x[[n]]`, respectively. The primary exception
#' is data frames, where they instead retrieve rows, i.e. `x[1, ]`, `x[nrow(x),
#' ]`, and `x[n, ]`. This is consistent with the tidyverse/vctrs principle which
#' treats data frames as a vector of rows, rather than a vector of columns.
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
#'   When `x` is a list , `default` is allowed to be any value. There are no
#'   type or size restrictions in this case.
#' @param na_rm Should missing values in `x` be removed before extracting the
#'   value?
#'
#' @return
#' If `x` is a list, a single element from that list. Otherwise, a vector the
#' same type as `x` with size 1.
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
#' # This out of bounds behavior also applies to empty vectors
#' first(integer())
#'
#' # You can customize the default value with `default`
#' nth(x, 11, default = -1L)
#' first(integer(), default = 0L)
#'
#' # `order_by` provides optional ordering
#' last(x)
#' last(x, order_by = y)
#'
#' # `na_rm` removes missing values before extracting the value
#' z <- c(NA, NA, 1, 3, NA, 5, NA)
#' first(z)
#' first(z, na_rm = TRUE)
#' last(z, na_rm = TRUE)
#' nth(z, 3, na_rm = TRUE)
#'
#' # For data frames, these select entire rows
#' df <- tibble(a = 1:5, b = 6:10)
#' first(df)
#' nth(df, 4)
nth <- function(x, n, order_by = NULL, default = NULL, na_rm = FALSE) {
  size <- vec_size(x)

  vec_check_size(n, size = 1L)
  n <- vec_cast(n, to = integer())

  if (!is.null(order_by)) {
    vec_check_size(order_by, size = size)
  }

  default <- check_nth_default(default, x = x)

  check_bool(na_rm)

  if (na_rm && vec_any_missing(x)) {
    not_missing <- !vec_detect_missing(x)

    x <- vec_slice(x, not_missing)
    size <- vec_size(x)

    if (!is.null(order_by)) {
      order_by <- vec_slice(order_by, not_missing)
    }
  }

  if (is.na(n)) {
    abort("`n` can't be `NA`.")
  }

  if (n < 0L) {
    # Negative values index from RHS
    n <- size + n + 1L
  }

  if (n <= 0L || n > size) {
    return(default)
  }

  if (!is.null(order_by)) {
    order <- vec_order_radix(order_by)
    n <- order[[n]]
  }

  vec_slice2(x, n)
}

#' @export
#' @rdname nth
first <- function(x, order_by = NULL, default = NULL, na_rm = FALSE) {
  nth(x, 1L, order_by = order_by, default = default, na_rm = na_rm)
}

#' @export
#' @rdname nth
last <- function(x, order_by = NULL, default = NULL, na_rm = FALSE) {
  nth(x, -1L, order_by = order_by, default = default, na_rm = na_rm)
}

check_nth_default <- function(default, x, ..., error_call = caller_env()) {
  check_dots_empty0(...)

  if (obj_is_list(x)) {
    # Very special behavior for lists, since we use `[[` on them.
    # Valid to use any `default` here (even non-vectors).
    # And `default = NULL` is the correct default `default` for lists.
    return(default)
  }

  if (is.null(default)) {
    return(vec_init(x))
  }

  vec_check_size(default, size = 1L, call = error_call)

  default <- vec_cast(
    x = default,
    to = x,
    x_arg = "default",
    to_arg = "x",
    call = error_call
  )

  default
}

vec_slice2 <- function(x, i) {
  # Our unimplemented vctrs equivalent of `[[`
  # https://github.com/r-lib/vctrs/pull/1228/

  # A real implementation would use this, but it is too slow right now
  # and we know `i` is a valid integer index (#6682)
  # i <- vec_as_location2(i, vec_size(x))

  if (obj_is_list(x)) {
    out <- .subset2(x, i)
  } else {
    out <- vec_slice(x, i)
    out <- vec_set_names(out, NULL)
  }

  out
}
