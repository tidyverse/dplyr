#' Vectorised if.
#'
#' Compared to the base \code{\link{ifelse}()}, this function is more strict.
#' It checks that \code{true} and \code{false} are the same type. This
#' strictness makes the output type more predictable, and makes it somewhat
#' faster.
#'
#' @param condition Logical vector
#' @param true,false Values to use for \code{TRUE} and \code{FALSE} values of
#'   \code{condition}. They must be either the same length as \code{condition},
#'   or length 1. They must also be the same type: \code{if_else} checks that
#'   they have the same type and same class. All other attributes are
#'   taken from \code{true}.
#' @param missing If not \code{NULL}, will be used to replace missing
#'   values.
#' @return Where \code{condition} is \code{TRUE}, the matching value from
#'   \code{true}, where it's \code{FALSE}, the matching value from \code{false},
#'   otherwise \code{NA}.
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
    stop("`condition` must be logical", call. = FALSE)
  }

  out <- true[rep(NA_integer_, length(condition))]
  out <- replace_with(out, condition & !is.na(condition), true, "`true`")
  out <- replace_with(out, !condition & !is.na(condition), false, "`false`")
  out <- replace_with(out, is.na(condition), missing, "`missing`")

  out
}
