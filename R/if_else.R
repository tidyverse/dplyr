#' Vectorised if
#'
#' Compared to the base [ifelse()], this function is more strict.
#' It checks that `true` and `false` are the same type. This
#' strictness makes the output type more predictable, and makes it somewhat
#' faster.
#'
#' @param condition Logical vector
#' @param true,false Values to use for `TRUE` and `FALSE` values of
#'   `condition`. They must be either the same length as `condition`,
#'   or length 1. They must also be the same type: `if_else()` checks that
#'   they have the same type and same class. All other attributes are
#'   taken from `true`.
#' @param missing If not `NULL`, will be used to replace missing
#'   values.
#' @return Where `condition` is `TRUE`, the matching value from
#'   `true`, where it's `FALSE`, the matching value from `false`,
#'   otherwise `NA`.
#' @export
#' @examples
#' x <- c(-5:5, NA)
#' if_else(x < 0, NA_integer_, x)
#' if_else(x < 0, "negative", "positive", "missing")
#'
#' # Unlike ifelse, if_else preserves types
#' x <- factor(sample(letters[1:5], 10, replace = TRUE))
#' ifelse(x %in% c("a", "b", "c"), x, factor(NA))
#' if_else(x %in% c("a", "b", "c"), x, factor(NA))
#' # Attributes are taken from the `true` vector,
if_else <- function(condition, true, false, missing = NULL) {
  if (!is.logical(condition)) {
    bad_args("condition", "must be a logical vector, not {friendly_type_of(condition)}.")
  }

  out <- true[rep(NA_integer_, length(condition))]
  out <- replace_with(
    out, condition, true,
    fmt_args(~ true),
    glue("length of {fmt_args(~condition)}")
  )
  out <- replace_with(
    out, !condition, false,
    fmt_args(~ false),
    glue("length of {fmt_args(~condition)}")
  )
  out <- replace_with(
    out, is.na(condition), missing,
    fmt_args(~ missing),
    glue("length of {fmt_args(~condition)}")
  )

  out
}
