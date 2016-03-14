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
#' @return Where \code{condition} is \code{TRUE}, the matching value from
#'   \code{true}, where it's \code{FALSE}, the matching value from \code{false},
#'   otherwise \code{NA}.
#' @export
#' @examples
#' x <- -10:10
#' ifelse(x < 0, NA, x)
#'
#' # Unlike ifelse, if_else preserves types
#' x <- factor(sample(letters[1:5], 10, replace = TRUE))
#' ifelse(x %in% c("a", "b", "c"), x, factor(NA))
#' if_else(x %in% c("a", "b", "c"), x, factor(NA))
#' # Attributes are taken from the `true` vector,
if_else <- function(condition, true, false) {
  if (!is.logical(condition)) {
    stop("`condition` must be logical", call. = FALSE)
  }
  if (length(true) != length(condition) && length(true) != 1L) {
    stop("`true` must be length one or the same length as `condition`", call. = FALSE)
  }
  if (length(false) != length(condition) && length(false) != 1L) {
    stop("`false` must be length one or the same length as `condition`", call. = FALSE)
  }
  if (typeof(false) != typeof(true)) {
    stop(
      "`true` and `false` must be the same type ",
      "(", typeof(false), " vs ", typeof(true), ")",
      call. = FALSE
    )
  }
  if (!identical(class(false), class(true))) {
    stop(
      "`true` and `false` must have same class ",
      "(", paste(class(false), collapse = "/"), " vs ", paste(class(true), collapse = "/"), ")",
      call. = FALSE
    )
  }

  out <- true[rep(NA_integer_, length(condition))]

  is_true <- condition & !is.na(condition)
  if (length(true) == 1L) {
    out[is_true] <- true
  } else {
    out[is_true] <- true[is_true]
  }

  is_false <- !condition & !is.na(condition)
  if (length(false) == 1L) {
    out[is_false] <- false
  } else {
    out[is_false] <- false[is_false]
  }

  out
}
