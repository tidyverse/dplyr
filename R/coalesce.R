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
#' @seealso [na_if()] to replace specified values with an `NA`.
#'   [tidyr::replace_na()] to replace `NA` with a value.
#'
#' @export
#' @examples
#' # Use a single value to replace all missing values
#' x <- sample(c(1:5, NA, NA, NA))
#' coalesce(x, 0L)
#'
#' # The equivalent to a missing value in a list is `NULL`
#' coalesce(list(1, 2, NULL), list(NA))
#'
#' # Or generate a complete vector from partially missing pieces
#' y <- c(1, 2, NA, NA, 5)
#' z <- c(NA, NA, 3, 4, 5)
#' coalesce(y, z)
#'
#' # Supply lists by splicing them into dots:
#' vecs <- list(
#'   c(1, 2, NA, NA, 5),
#'   c(NA, NA, 3, 4, 5)
#' )
#' coalesce(!!!vecs)
coalesce <- function(..., .ptype = NULL, .size = NULL) {
  args <- list2(...)

  if (vec_any_missing(args)) {
    # Drop `NULL`s
    not_missing <- !vec_detect_missing(args)
    args <- vec_slice(args, not_missing)
  }

  if (length(args) == 0L) {
    abort("`...` can't be empty.")
  }

  # Recycle early so logical conditions computed below will be the same length,
  # as required by `vec_case_when()`
  args <- vec_recycle_common(!!!args, .size = .size)

  # Name early to get correct indexing in `vec_case_when()` error messages
  names <- names2(args)
  names <- names_as_error_names(names)
  args <- set_names(args, names)

  conditions <- map(args, ~{
    !vec_detect_missing(.x)
  })

  vec_case_when(
    conditions = conditions,
    values = args,
    conditions_arg = "",
    values_arg = "",
    ptype = .ptype,
    size = .size,
    call = current_env()
  )
}
