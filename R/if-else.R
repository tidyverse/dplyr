#' Vectorised if-else
#'
#' `if_else()` is a vectorized [if-else][if]. Compared to the base R equivalent,
#' [ifelse()], this function allows you to handle missing values in the
#' `condition` with `missing` and always takes `true`, `false`, and `missing`
#' into account when determining what the output type should be.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param condition A logical vector
#'
#' @param true,false Vectors to use for `TRUE` and `FALSE` values of
#'   `condition`.
#'
#'   Both `true` and `false` will be [recycled][vctrs::theory-faq-recycling]
#'   to the size of `condition`.
#'
#'   `true`, `false`, and `missing` (if used) will be cast to their common type.
#'
#' @param missing If not `NULL`, will be used as the value for `NA` values of
#'   `condition`. Follows the same size and type rules as `true` and `false`.
#'
#' @param ptype An optional prototype declaring the desired output type. If
#'   supplied, this overrides the common type of `true`, `false`, and `missing`.
#'
#' @param size `r lifecycle::badge("deprecated")`
#'
#'   Output size is always taken from `condition`.
#'
#' @return
#' A vector with the same size as `condition` and the same type as the common
#' type of `true`, `false`, and `missing`.
#'
#' Where `condition` is `TRUE`, the matching values from `true`, where it is
#' `FALSE`, the matching values from `false`, and where it is `NA`, the matching
#' values from `missing`, if provided, otherwise a missing value will be used.
#'
#' @seealso [vctrs::vec_if_else()]
#'
#' @export
#' @examples
#' x <- c(-5:5, NA)
#' if_else(x < 0, NA, x)
#'
#' # Explicitly handle `NA` values in the `condition` with `missing`
#' if_else(x < 0, "negative", "positive", missing = "missing")
#'
#' # Unlike `ifelse()`, `if_else()` preserves types
#' x <- factor(sample(letters[1:5], 10, replace = TRUE))
#' ifelse(x %in% c("a", "b", "c"), x, NA)
#' if_else(x %in% c("a", "b", "c"), x, NA)
#'
#' # `if_else()` is often useful for creating new columns inside of `mutate()`
#' starwars |>
#'   mutate(category = if_else(height < 100, "short", "tall"), .keep = "used")
if_else <- function(
  condition,
  true,
  false,
  missing = NULL,
  ...,
  ptype = NULL,
  size = deprecated()
) {
  check_dots_empty0(...)

  if (!is_missing(size)) {
    lifecycle::deprecate_warn(
      when = "1.2.0",
      what = "if_else(size = )",
      id = "dplyr-if-else-size"
    )
  }

  vec_if_else(
    condition = condition,
    true = true,
    false = false,
    missing = missing,
    ptype = ptype,
    error_call = current_env()
  )
}
