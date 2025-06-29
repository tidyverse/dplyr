#' Recode or update `from` one value `to` another
#'
#' @description
#'
#' - `vec_recode()` constructs an entirely new vector by recoding the values in
#'   `x` using the relationship specified by `from` and `to`. If there are
#'   values in `x` not matched by `from`, then they are recoded to the `default`
#'   value.
#'
#' - `vec_update()` updates values in `x` using the relationship specified by
#'   `from` and `to`. In this case, `to` must have the same type as `x` and
#'   values in `x` not matched by `from` pass through untouched.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x A vector.
#'
#' @param from Values present in `x` to recode to values in `to`.
#'
#'   Values present in `from` but not in `x` are ignored.
#'
#'   Values present in `x` but not in `from` are recoded to `default`.
#'
#'   `from` will be [cast][vctrs::theory-faq-coercion] to the type of `x`.
#'
#' @param to Values to recode values in `x` matched by `from` to.
#'
#'   Must be the same size as `from`, or size 1.
#'
#'   The common type of `to` and `default` will determine the type of the
#'   output, unless `ptype` is provided.
#'
#' @param default Default value to use when there is a value present in `x`
#'   that isn't matched by a value in `from`.
#'
#'   By default, a missing value is used as the default value.
#'
#'   If supplied, `default` must be the same size as `x` or size 1.
#'
#' @param x_arg,from_arg,to_arg,default_arg Argument names used in error
#'   messages.
#'
#' @param ptype An optional override for the output type, which is usually
#'   computed as the common type of `to` and `default`.
#'
#' @returns
#' A vector the same size `x`.
#'
#' - For `vec_recode()`, the type of the output is computed as the common type
#'   of `to` and `default`, unless overriden by `ptype`.
#'
#' - For `vec_update()`, the type of the output will have the same type as `x`.
#'
#' @name vec-recode-and-update
#'
#' @examples
#' x <- c(1, 2, 3, 1, 2, 4, NA, 5)
#'
#' # Imagine you have a pre-existing mapping stored in two vectors
#' from <- c(1, 2, 3, 4, 5)
#' to <- c("a", "b", "c", "d", "e")
#' vec_recode(x, from = from, to = to)
#'
#' # If you don't map all of the values, a `default` is used
#' from <- c(1, 2, 3)
#' to <- c("a", "b", "c")
#' vec_recode(x, from = from, to = to)
#' vec_recode(x, from = from, to = to, default = "unknown")
#'
#' # If you want to partially update `x`, retaining the type of `x` and
#' # leaving values not covered by `from` alone, use `vec_update()`
#' from <- c(1, 2, 3)
#' to <- -from
#' vec_update(x, from = from, to = to)
#'
#' # This is also a useful way to quickly map from or to `NA`
#' vec_update(x, from = NA, to = 0)
#' vec_update(x, from = c(1, 2, 3), to = NA)
#'
#' @noRd
NULL

# #' @rdname vec-recode-and-update
vec_recode <- function(
  x,
  ...,
  from,
  to,
  default = NULL,
  x_arg = "x",
  from_arg = "from",
  to_arg = "to",
  default_arg = "default",
  ptype = NULL,
  call = current_env()
) {
  check_dots_empty0(...)

  obj_check_vector(x, arg = x_arg, call = call)
  obj_check_vector(from, arg = from_arg, call = call)
  obj_check_vector(to, arg = to_arg, call = call)
  if (!is.null(default)) {
    obj_check_vector(default, arg = default_arg, call = call)
  }

  # Be fairly strict about typing, don't take common type in `vec_match()`.
  # It "feels" right to require the type of `x` for `from`, rather than a
  # common type of the two.
  from <- vec_cast(
    x = from,
    to = x,
    x_arg = from_arg,
    to_arg = x_arg,
    call = call
  )

  # Find locations where `to` will be used to recode.
  # Want `na_equal = TRUE` for `vec_recode(x, from = NA, to = to)`.
  loc <- vec_match(
    needles = x,
    haystack = from,
    na_equal = TRUE,
    needles_arg = x_arg,
    haystack_arg = from_arg
  )

  x_size <- vec_size(loc)

  # TODO: Could be a little faster and more memory efficient with C.
  # - Loop over `loc` tallying `n_missing`
  # - Allocate `loc_for_default` and `loc_for_to` of correct sizes
  # - Loop over `loc` again, filling `loc_for_default` and `loc_for_to`
  # TODO: And could skip the first loop tallying `n_missing` if we had an
  # internal version of `vec_match()` that returned the number of matches
  # in addition to their locations.
  loc_for_default <- vec_detect_missing(loc)
  loc_for_to <- !loc_for_default

  loc_for_default <- which(loc_for_default)
  loc_for_to <- which(loc_for_to)

  # Finalize `ptype`, allow `default` to participate in common type determination,
  # like in `vec_case_when()`
  everything <- list(to, default)
  names(everything) <- c(to_arg, default_arg)
  ptype <- vec_ptype_common(!!!everything, .ptype = ptype)

  to <- vec_cast(
    x = to,
    to = ptype,
    x_arg = to_arg,
    call = call
  )

  if (is.null(default)) {
    default <- vec_init(ptype)
  } else {
    default <- vec_cast(default, ptype)
  }

  # TODO: Could use `vec_slice_unsafe()` at the C level since location
  # vectors are created by us.
  if (vec_size(to) != 1L) {
    vec_check_size(to, size = vec_size(from), arg = to_arg, call = call)
    loc_from_to <- vec_slice(loc, loc_for_to)
    to <- vec_slice(to, loc_from_to)
  }

  if (vec_size(default) != 1L) {
    vec_check_size(default, size = x_size, arg = default_arg, call = call)
    default <- vec_slice(default, loc_for_default)
  }

  # TODO: If `list_unchop()` got an explicit `size` argument, then
  # we would not have to supply `default` or `use_default` at all in the
  # `default = NULL` case, which is fairly common.
  list_unchop(
    list(to, default),
    indices = list(loc_for_to, loc_for_default),
    ptype = ptype
  )
}

# #' @rdname vec-recode-and-update
vec_update <- function(
  x,
  ...,
  from,
  to,
  x_arg = "x",
  from_arg = "from",
  to_arg = "to",
  call = current_env()
) {
  check_dots_empty0(...)

  obj_check_vector(x, arg = x_arg, call = call)

  default <- x
  ptype <- vec_ptype_finalise(vec_ptype(x))

  vec_recode(
    x = x,
    from = from,
    to = to,
    default = default,
    x_arg = x_arg,
    from_arg = from_arg,
    to_arg = to_arg,
    default_arg = "default",
    ptype = ptype,
    call = call
  )
}
