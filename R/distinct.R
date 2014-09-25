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

#' @export
distinct_.data.frame <- function(.data, ..., .dots) {
  dist <- distinct_vars(.data, ..., .dots = .dots)
  distinct_impl(dist$data, dist$vars)
}

#' @export
distinct_.tbl_df <- function(.data, ..., .dots) {
  tbl_df(NextMethod())
}

#' @export
distinct_.grouped_df <- function(.data, ..., .dots) {
  groups <- lazyeval::as.lazy_dots(groups(.data))
  dist <- distinct_vars(.data, ..., .dots = c(.dots, groups))

  grouped_df(distinct_impl(dist$data, dist$vars), groups(.data))
}

#' @export
distinct_.data.table <- function(.data, ..., .dots) {
  dist <- distinct_vars(.data, ..., .dots = .dots)

  if (length(dist$vars) == 0) {
    unique(dist$data)
  } else {
    unique(dist$data, by = dist$vars)
  }
}

#' @export
distinct_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
distinct_.grouped_dt <- function(.data, ..., .dots) {
  groups <- lazyeval::as.lazy_dots(groups(.data))
  dist <- distinct_vars(.data, ..., .dots = c(.dots, groups))

  grouped_dt(unique(dist$data, by = dist$vars), groups(.data), copy = FALSE)
}

#' @export
distinct_.tbl_sql <- function(.data, ..., .dots) {
  dist <- distinct_vars(.data, ..., .dots = .dots)
  if (length(dist$vars) > 0) {
    stop("Can't calculate distinct only on specified columns with SQL",
      call. = FALSE)
  }

  from <- sql_subquery(dist$data$src$con, dist$data$query$sql)
  sql <- build_sql("SELECT DISTINCT * FROM ", from, con = dist$data$src$con)
  update(tbl(dist$data$src, sql, vars = dist$data$select), group_by = groups(.data))
}
