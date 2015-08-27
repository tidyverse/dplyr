#' Select distinct/unique rows.
#'
#' Retain only unique/distinct rows from an input tbl. This is an
#' efficient version of \code{\link{unique}}. \code{distinct()} is best-suited
#' for interactive use, \code{distinct_()} for calling from a function.
#'
#' @param .data a tbl
#' @param ... Variables to use when determining uniqueness. If there
#'   are multiple rows for a given combination of inputs, only the first
#'   row will be preserved.
#' @inheritParams filter
#' @export
#' @examples
#' df <- data.frame(
#'   x = sample(10, 100, rep = TRUE),
#'   y = sample(10, 100, rep = TRUE)
#' )
#' nrow(df)
#' nrow(distinct(df))
#' distinct(df, x)
#' distinct(df, y)
#'
#' # You can also use distinct on computed variables
#' distinct(df, diff = abs(x - y))
distinct <- function(.data, ...) {
  distinct_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname distinct
distinct_ <- function(.data, ..., .dots) {
  UseMethod("distinct_")
}

#' Same basic philosophy as group_by: lazy_dots comes in, list of data and
#' vars (character vector) comes out.
#' @noRd
distinct_vars <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  # If any calls, use mutate to add new columns, then distinct on those
  needs_mutate <- vapply(dots, function(x) !is.name(x$expr), logical(1))
  if (any(needs_mutate)) {
    .data <- mutate_(.data, .dots = dots[needs_mutate])
  }

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  list(data = .data, vars = names(dots))
}

#' Efficiently count the number of unique values in a set of vector
#'
#' This is a faster and more concise equivalent of \code{length(unique(x))}
#'
#' @param \dots vectors of values
#' @param na.rm id \code{TRUE} missing values don't count
#' @examples
#' x <- sample(1:10, 1e5, rep = TRUE)
#' length(unique(x))
#' n_distinct(x)
#' @export
n_distinct <- function(..., na.rm = FALSE){
  n_distinct_multi(list(...), na.rm)
}
