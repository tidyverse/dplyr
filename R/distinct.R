#' Select distinct/unique rows.
#'
#' Retain only unique/distinct rows from an input tbl. This is similar
#' to \code{\link{unique.data.frame}}, but considerably faster.
#'
#' @param .data a tbl
#' @param ... Optional variables to use when determining uniqueness. If there
#'   are multiple rows for a given combination of inputs, only the first
#'   row will be preserved.
#' @param .keep_all If \code{TRUE}, keep all variables in \code{.data}.
#'   If a combination of \code{...} is not distinct, this keeps the
#'   first row of values.
#' @inheritParams filter
#' @export
#' @examples
#' df <- data.frame(
#'   x = sample(10, 100, rep = TRUE),
#'   y = sample(10, 100, rep = TRUE)
#' )
#' nrow(df)
#' nrow(distinct(df, x, y))
#'
#' distinct(df, x)
#' distinct(df, y)
#'
#' # Can choose to keep all other variables as well
#' distinct(df, x, .keep_all = TRUE)
#' distinct(df, y, .keep_all = TRUE)
#'
#' # You can also use distinct on computed variables
#' distinct(df, diff = abs(x - y))
distinct <- function(.data, ..., .keep_all = FALSE) {
  distinct_(.data, .dots = lazyeval::lazy_dots(...), .keep_all = .keep_all)
}

#' @export
#' @rdname distinct
distinct_ <- function(.data, ..., .dots, .keep_all = FALSE) {
  UseMethod("distinct_")
}

#' Same basic philosophy as group_by: lazy_dots comes in, list of data and
#' vars (character vector) comes out.
#' @noRd
distinct_vars <- function(.data, ..., .dots, .keep_all = FALSE) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  # If any calls, use mutate to add new columns, then distinct on those
  needs_mutate <- vapply(dots, function(x) !is.name(x$expr), logical(1))
  if (any(needs_mutate)) {
    .data <- mutate_(.data, .dots = dots[needs_mutate])
  }

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  vars <- names(dots)

  if (.keep_all) {
    keep <- names(.data)
  } else {
    keep <- vars
  }
  if (length(keep) == 0) {
    stop("No variables selected", call. = FALSE)
  }

  list(data = .data, vars = vars, keep = keep)
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
