#' Select
#'
#' Retain only unique/distinct variables from an input tbl. This is an
#' efficient version of \code{\link{unique}}. \code{distinct()} is best-suited
#' for interactive use, \code{distinct_()} for calling from a function.
#'
#' @param .data a tbl
#' @param ...,vars Variables to use when determining uniqueness. If there
#'   are multiple rows for a given combination of inputs, only the first
#'   row will be preserved.
#'
#'   \code{...} should be an unquoted, comma-separated list of variable
#'   names. \code{vars} can be either a character vector of column names,
#'   or a list of symbols.
#' @export
#' @examples
#' df <- data.frame(
#'   x = sample(10, 100, rep = TRUE),
#'   y = sample(10, 100, rep = TRUE)
#' )
#' nrow(df)
#' nrow(distinct(df))
distinct <- function(.data, ...) {
  distinct_(.data, dots(...))
}

#' @export
#' @rdname distinct
distinct_ <- function(.data, vars = character()) {
  UseMethod("distinct_")
}

#' @export
distinct_.data.frame <- function(.data, vars = character()) {
  vars <- standardise_vars(vars)
  distinct_impl(.data)
}

#' @export
distinct_.grouped_df <- function(.data, vars = character()) {
  if (length(vars) > 0) {
    vars <- c(standardise_vars(groups(.data)), standardise_vars(vars))
  }

  grouped_df(distinct_.data.frame(.data, vars = vars), groups(.data))
}

#' @export
distinct_.data.table <- function(.data, vars = character()) {
  vars <- standardise_vars(vars)
  if (length(vars) == 0) {
    unique(.data)
  } else {
    unique(.data, by = vars)
  }
}

#' @export
distinct_.tbl_dt <- function(.data, vars = character()) {
  tbl_dt(NextMethod())
}


#' @export
distinct_.grouped_dt <- function(.data, vars = character()) {
  if (length(vars) > 0) {
    vars <- c(standardise_vars(groups(.data)), standardise_vars(vars))
  }

  grouped_df(distinct_.data.table(.data, vars = vars), groups(.data))
}

#' @export
distinct_.tbl_sql <- function(.data, vars = character()) {
  if (length(vars) > 0) {
    stop("Can't calculate distinct only on specified columns with SQL",
      call. = FALSE)
  }

  from <- sql_subquery(.data$src$con, .data$query$sql)
  sql <- build_sql("SELECT DISTINCT * FROM ", from, con = .data$src$con)
  update(tbl(.data$src, sql, vars = .data$select), group_by = groups(.data))
}

standardise_vars <- function(vars) {
  if (is.character(vars)) {
    vars
  } else if (is.list(vars)) {
    vapply(vars, as.character, character(1))
  } else {
    stop("Unknown variable specification", call. = FALSE)
  }
}
