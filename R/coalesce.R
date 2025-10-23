#' Find the first non-missing element
#'
#' Given a set of vectors, `coalesce()` finds the first non-missing value at
#' each position. It's inspired by the SQL `COALESCE` function which does the
#' same thing for SQL `NULL`s.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'
#'   One or more vectors. These will be
#'   [recycled][vctrs::theory-faq-recycling] against each other, and will be
#'   cast to their common type.
#'
#' @param .ptype An optional prototype declaring the desired output type. If
#'   supplied, this overrides the common type of the vectors in `...`.
#'
#' @param .size An optional size declaring the desired output size. If supplied,
#'   this overrides the common size of the vectors in `...`.
#'
#' @return A vector with the same type and size as the common type and common
#'   size of the vectors in `...`.
#'
#' @seealso
#'
#'   - [na_if()] to replace a specified value with `NA`.
#'
#'   - [replace_values()] for making arbitrary replacements by value.
#'
#'   - [replace_when()] for making arbitrary replacements using logical
#'     conditions.
#'
#' @export
#' @examples
#' # Replace missing values with a single value
#' x <- sample(c(1:5, NA, NA, NA))
#' coalesce(x, 0L)
#'
#' # Or replace missing values with the corresponding non-missing value in
#' # another vector
#' x <- c(1, 2, NA, NA, 5, NA)
#' y <- c(NA, NA, 3, 4, 5, NA)
#' coalesce(x, y)
#'
#' # For cases like these where your replacement is a single value or a single
#' # vector, `replace_values()` works just as well
#' replace_values(x, NA ~ 0)
#' coalesce(x, 0)
#'
#' replace_values(x, NA ~ y)
#' coalesce(x, y)
#'
#' # `coalesce()` really shines when you have >2 vectors to coalesce with
#' z <- c(NA, 2, 3, 4, 5, 6)
#' coalesce(x, y, z)
#'
#' # If you're looking to replace values with `NA`, rather than replacing `NA`
#' # with a value, then use `replace_values()`
#' x <- c(0, -1, 5, -99, 8)
#' replace_values(x, c(-1, -99) ~ NA)
#'
#' # The equivalent to a missing value in a list is `NULL`
#' coalesce(list(1, 2, NULL, NA), list(0))
#'
#' # Supply lists of vectors by splicing them into dots
#' vecs <- list(
#'   c(1, 2, NA, NA, 5),
#'   c(NA, NA, 3, 4, 5)
#' )
#' coalesce(!!!vecs)
coalesce <- function(..., .ptype = NULL, .size = NULL) {
  args <- list2(...)

  if (length(args) == 0L) {
    abort("`...` can't be empty.")
  }
  if (vec_all_missing(args)) {
    abort("`...` must contain at least 1 non-`NULL` value.")
  }

  # We do vector, type, and size checks up front before dropping any `NULL`
  # values or extracting out a `default` to ensure that any errors report
  # the correct index
  list_check_all_vectors(args, allow_null = TRUE, arg = "")

  .ptype <- vec_ptype_common(!!!args, .ptype = .ptype)
  args <- vec_cast_common(!!!args, .to = .ptype)

  if (is_null(.size)) {
    .size <- vec_size_common(!!!args)
  } else {
    # Check recyclability, but delay actual recycling
    list_check_all_recyclable(args, .size, allow_null = TRUE, arg = "")
  }

  # From this point on we don't expect any errors

  args <- convert_from_coalesce_to_case_when(args, .size)
  values <- args$values
  default <- args$default

  conditions <- map(values, function(value) {
    !vec_detect_missing(value)
  })

  vec_case_when(
    conditions = conditions,
    values = values,
    default = default,
    ptype = .ptype,
    size = .size
  )
}

# Goal is to convert from `...` of `coalesce()` to `values` and `default`
# of `vec_case_when()`
#
# Recognize that these are equivalent:
#
# ```
# coalesce(x, y)
# case_when(!vec_detect_missing(x) ~ x, !vec_detect_missing(y) ~ y)
#
# coalesce(x, y_with_no_missings)
# case_when(!vec_detect_missing(x) ~ x, .default = y_with_no_missings)
#
# coalesce(x, NULL, y, 0)
# case_when(!vec_detect_missing(x) ~ x, !vec_detect_missing(y) ~ y, .default = 0)
# ```
#
# Note how the last element can be used as `default` if it doesn't contain any
# missing values. This is a very nice optimization since `vec_case_when()`
# doesn't need to recycle that value, and efficiently computes its output
# locations!
#
# Note how `NULL`s are dropped during the conversion.
convert_from_coalesce_to_case_when <- function(args, size) {
  if (vec_any_missing(args)) {
    # Drop `NULL`
    args <- vec_slice(args, vec_detect_complete(args))
  }

  args_size <- length(args)

  if (args_size == 0L) {
    abort("Checked for at least 1 non-`NULL` value earlier", .internal = TRUE)
  }

  # Try to promote the `last` element of `args` to `default`
  #
  # For the 99% case of `coalesce(x, 0)`, this:
  # - Avoids recycling `0` to size `size`.
  # - Avoids computing `!vec_detect_missing()` on that recycled `0`.
  #
  # Can only do this if the `last` element doesn't contain missing values
  # due to how names are handled. We don't want to take the name from any `NA`
  # element, which is what would happen if we promoted the whole `y` vector here
  # to `default`.
  #
  # ```
  # x <- c(a = NA, b = 2)
  # y <- c(c = NA, d = 4)
  #
  # coalesce(x, y)
  # # Want c(NA, b = 2)
  # # Not c(c = NA, b = 2)
  #
  # # Compare to
  # case_when(!vec_detect_missing(x) ~ x, !vec_detect_missing(y) ~ y)
  # case_when(!vec_detect_missing(x) ~ x, .default = y)
  # ```
  last <- args[[args_size]]

  if (vec_any_missing(last)) {
    default <- NULL
  } else {
    default <- last
    args <- args[-args_size]
  }

  # Most of the time this recycle is a no-op. Two cases where it isn't:
  # - `coalesce(x, 0, 1)`, where `1` becomes `default` but we still have a
  #   scalar `0`.
  # - `coalesce(x, NA)`, where `NA` can't be promoted, so we have a scalar `NA`.
  args <- vec_recycle_common(!!!args, .size = size)

  list(values = args, default = default)
}

vec_all_missing <- function(x) {
  if (!vec_any_missing(x)) {
    return(FALSE)
  }
  sum(vec_detect_missing(x)) == vec_size(x)
}
