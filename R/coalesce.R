#' Find first non-missing element
#'
#' Given a set of vectors, `coalesce()` finds the first non-missing value
#' at each position. This is inspired by the SQL `COALESCE` function
#' which does the same thing for `NULL`s.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Vectors. Inputs should be
#'    recyclable (either be length 1 or same length as the longest vector) and
#'    coercible to a common type.
#' @return A vector the same length as the first `...` argument with
#'   missing values replaced by the first non-missing value.
#' @seealso [na_if()] to replace specified values with a `NA`.
#' [tidyr::replace_na()] to replace `NA` with a value
#' @export
#' @examples
#' # Use a single value to replace all missing values
#' x <- sample(c(1:5, NA, NA, NA))
#' coalesce(x, 0L)
#'
#' # Or match together a complete vector from missing pieces
#' y <- c(1, 2, NA, NA, 5)
#' z <- c(NA, NA, 3, 4, 5)
#' coalesce(y, z)
#'
#' # Supply lists by with dynamic dots
#' vecs <- list(
#'   c(1, 2, NA, NA, 5),
#'   c(NA, NA, 3, 4, 5)
#' )
#' coalesce(!!!vecs)
coalesce <- function(...) {
  if (missing(..1)) {
    abort("At least one argument must be supplied.")
  }

  values <- list2(...)
  values <- vec_cast_common(!!!values)
  values <- vec_recycle_common(!!!values)

  x <- values[[1]]
  values <- values[-1]

  for (i in seq_along(values)) {
    x_miss <- is.na(x)
    vec_slice(x, x_miss) <- vec_slice(values[[i]], x_miss)
  }
  x
}
