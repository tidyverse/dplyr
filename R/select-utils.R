#' Select helpers
#'
#' These functions allow you to select variables based on their names.
#' * `starts_with()`: starts with a prefix
#' * `ends_with()`: ends with a prefix
#' * `contains()`: contains a literal string
#' * `matches()`: matches a regular expression
#' * `num_range()`: a numerical range like x01, x02, x03.
#' * `one_of()`: variables in character vector.
#' * `everything()`: all variables.
#'
#' @param match A string.
#' @param ignore.case If `TRUE`, the default, ignores case when matching
#'   names.
#' @param vars A character vector of variable names. When called from inside
#'   [select()] these are automatically set to the names of the
#'   table.
#' @name select_helpers
#' @return An integer vector giving the position of the matched variables.
#' @examples
#' iris <- tbl_df(iris) # so it prints a little nicer
#' select(iris, starts_with("Petal"))
#' select(iris, ends_with("Width"))
#' select(iris, contains("etal"))
#' select(iris, matches(".t."))
#' select(iris, Petal.Length, Petal.Width)
#' select(iris, everything())
#' vars <- c("Petal.Length", "Petal.Width")
#' select(iris, one_of(vars))
NULL

cur_vars_env <- child_env(NULL)

set_current_vars <- function(x) {
  stopifnot(is_character(x) || is_null(x))

  old <- cur_vars_env$selected
  cur_vars_env$selected <- x

  invisible(old)
}

#' @export
#' @rdname select_helpers
current_vars <- function() {
  cur_vars_env$selected %||% abort("Variable context not set")
}

#' @export
#' @rdname select_helpers
starts_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is_string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  which_vars(match, substr(vars, 1, n))
}

#' @export
#' @rdname select_helpers
ends_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is_string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  length <- nchar(vars)

  which_vars(match, substr(vars, pmax(1, length - n + 1), length))
}

#' @export
#' @rdname select_helpers
contains <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is_string(match), nchar(match) > 0)

  if (ignore.case) {
    vars <- tolower(vars)
    match <- tolower(match)
  }
  grep_vars(match, vars, fixed = TRUE)
}

#' @export
#' @rdname select_helpers
matches <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is_string(match), nchar(match) > 0)

  grep_vars(match, vars, ignore.case = ignore.case)
}

#' @export
#' @rdname select_helpers
#' @param prefix A prefix that starts the numeric range.
#' @param range A sequence of integers, like `1:5`
#' @param width Optionally, the "width" of the numeric range. For example,
#'   a range of 2 gives "01", a range of three "001", etc.
num_range <- function(prefix, range, width = NULL, vars = current_vars()) {
  if (!is_null(width)) {
    range <- sprintf(paste0("%0", width, "d"), range)
  }
  match_vars(paste0(prefix, range), vars)
}

#' @export
#' @rdname select_helpers
#' @param ... One or more character vectors.
one_of <- function(..., vars = current_vars()) {
  keep <- c(...)

  if (!is_character(keep)) {
    bad("All arguments must be character vectors, not {type_of(keep)}")
  }

  if (!all(keep %in% vars)) {
    bad <- setdiff(keep, vars)
    warn(glue("Unknown variables: ", paste0("`", bad, "`", collapse = ", ")))
  }

  match_vars(keep, vars)
}

#' @export
#' @rdname select_helpers
everything <- function(vars = current_vars()) {
  seq_along(vars)
}

match_vars <- function(needle, haystack) {
  x <- match(needle, haystack)
  x[!is.na(x)]
}

grep_vars <- function(needle, haystack, ...) {
  grep(needle, haystack, ...)
}

which_vars <- function(needle, haystack) {
  which(needle == haystack)
}
