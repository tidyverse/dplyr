#' Select distinct/unique rows
#'
#' Retain only unique/distinct rows from an input tbl. This is similar
#' to [unique.data.frame()], but considerably faster.
#'
#' @param .data a tbl
#' @param ... Optional variables to use when determining uniqueness. If there
#'   are multiple rows for a given combination of inputs, only the first
#'   row will be preserved. If omitted, will use all variables.
#' @param .keep_all If `TRUE`, keep all variables in `.data`.
#'   If a combination of `...` is not distinct, this keeps the
#'   first row of values.
#' @inheritParams filter
#' @export
#' @examples
#' df <- tibble(
#'   x = sample(10, 100, rep = TRUE),
#'   y = sample(10, 100, rep = TRUE)
#' )
#' nrow(df)
#' nrow(distinct(df))
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
#'
#' # The same behaviour applies for grouped data frames
#' # except that the grouping variables are always included
#' df <- tibble(
#'   g = c(1, 1, 2, 2),
#'   x = c(1, 1, 2, 1)
#' ) %>% group_by(g)
#' df %>% distinct()
#' df %>% distinct(x)
distinct <- function(.data, ..., .keep_all = FALSE) {
  UseMethod("distinct")
}
#' @export
distinct.default <- function(.data, ..., .keep_all = FALSE) {
  distinct_(.data, .dots = compat_as_lazy_dots(...), .keep_all = .keep_all)
}
#' @export
#' @rdname se-deprecated
#' @inheritParams distinct
distinct_ <- function(.data, ..., .dots, .keep_all = FALSE) {
  UseMethod("distinct_")
}

#' Same basic philosophy as group_by: lazy_dots comes in, list of data and
#' vars (character vector) comes out.
#' @noRd
distinct_vars <- function(.data, vars, group_vars = character(), .keep_all = FALSE) {
  stopifnot(is_quosures(vars), is.character(group_vars))

  # If no input, keep all variables
  if (length(vars) == 0) {
    return(list(
      data = .data,
      vars = names(.data),
      keep = names(.data)
    ))
  }

  # If any calls, use mutate to add new columns, then distinct on those
  needs_mutate <- map_lgl(vars, quo_is_lang)
  if (any(needs_mutate)) {
    .data <- mutate(.data, !!! vars[needs_mutate])
  }

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  vars <- intersect(names(.data), c(names(vars), group_vars))

  if (.keep_all) {
    keep <- names(.data)
  } else {
    keep <- unique(vars)
  }

  list(data = .data, vars = vars, keep = keep)
}

#' Efficiently count the number of unique values in a set of vector
#'
#' This is a faster and more concise equivalent of `length(unique(x))`
#'
#' @param \dots vectors of values
#' @param na.rm id `TRUE` missing values don't count
#' @examples
#' x <- sample(1:10, 1e5, rep = TRUE)
#' length(unique(x))
#' n_distinct(x)
#' @export
n_distinct <- function(..., na.rm = FALSE) {
  n_distinct_multi(list(...), na.rm)
}
