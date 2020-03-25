#' Find first non-missing element
#'
#' Given a set of vectors, `coalesce()` finds the first non-missing value
#' at each position. This is inspired by the SQL `COALESCE` function
#' which does the same thing for `NULL`s.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Vectors. All inputs should either be length 1, or the
#'   same length as the first argument.
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
#' # Supply lists by splicing them into dots:
#' vecs <- list(
#'   c(1, 2, NA, NA, 5),
#'   c(NA, NA, 3, 4, 5)
#' )
#' coalesce(!!!vecs)
coalesce <- function(...) {
  if (missing(..1)) {
    abort("At least one argument must be supplied")
  }

  values <- list2(...)
  x <- values[[1]]
  values <- values[-1]

  for (i in seq_along(values)) {
    x <- replace_with(
      x, is.na(x), values[[i]],
      glue("Argument {i + 1}"),
      glue("length of {fmt_args(~x)}")
    )
  }
  x
}
