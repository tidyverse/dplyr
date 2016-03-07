#' Select helpers
#'
#' These functions allow you to select variables based on their names.
#' \itemize{
#'  \item \code{starts_with()}: starts with a prefix
#'  \item \code{ends_with()}: ends with a prefix
#'  \item \code{contains()}: contains a literal string
#'  \item \code{matches()}: matches a regular expression
#'  \item \code{num_range()}: a numerical range like x01, x02, x03.
#'  \item \code{one_of()}: varables in character vector.
#'  \item \code{everything()}: all variables.
#' }
#'
#' @param match A string.
#' @param ignore.case If \code{TRUE}, the default, ignores case when matching
#'   names.
#' @param vars A character vector of variable names. When called from inside
#'   \code{\link{select}()} these are automatically set to the names of the
#'   table.
#' @name select_helpers
#' @return An integer vector given the position of the matched variables.
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

cur_vars_env <- new.env()

set_current_vars <- function(x) {
  stopifnot(is.character(x))
  cur_vars_env$selected <- x
}
reset_current_vars <- function() {
  set_current_vars(character())
}

#' @export
#' @rdname select_helpers
current_vars <- function() cur_vars_env$selected

#' @export
#' @rdname select_helpers
starts_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  which(substr(vars, 1, n) == match)
}

#' @export
#' @rdname select_helpers
ends_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  length <- nchar(vars)

  which(substr(vars, pmax(1, length - n + 1), length) == match)
}

#' @export
#' @rdname select_helpers
contains <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), nchar(match) > 0)

  if (ignore.case) {
    vars <- tolower(vars)
    match <- tolower(match)
  }
  grep(match, vars, fixed = TRUE)
}

#' @export
#' @rdname select_helpers
matches <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), nchar(match) > 0)

  grep(match, vars, ignore.case = ignore.case)
}

#' @export
#' @rdname select_helpers
#' @param prefix A prefix that starts the numeric range.
#' @param range A sequence of integers, like \code{1:5}
#' @param width Optionally, the "width" of the numeric range. For example,
#'   a range of 2 gives "01", a range of three "001", etc.
num_range <- function(prefix, range, width = NULL, vars = current_vars()) {
  if (!is.null(width)) {
    range <- sprintf(paste0("%0", width, "d"), range)
  }
  match(paste0(prefix, range), vars)
}

#' @export
#' @rdname select_helpers
#' @param ... One or more character vectors.
one_of <- function(..., vars = current_vars()) {
  keep <- c(...)

  if (!is.character(keep)) {
    stop("Must be a vector of strings", call. = FALSE)
  }

  if (!all(keep %in% vars)) {
    bad <- setdiff(keep, vars)
    stop("Unknown variables: ", paste0("`", bad, "`", collapse = ", "))
  }

  match(keep, vars)
}

#' @export
#' @rdname select_helpers
everything <- function(vars = current_vars()) {
  seq_along(vars)
}
