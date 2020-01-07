#' Context dependent expressions
#'
#' @description
#' These functions return information about the "current" group or "current"
#' variable, so only work inside specific contexts
#'
#' * `n()` gives the current group size.
#' * `current_key()` gives the group keys, a tibble with one row and one column
#'   for each grouping variable.
#' * `current_column()` gives the current column being used in [across()].
#'
#' @examples
#' df <- data.frame(g = rep(1:3, 1:3), x = runif(6), y = runif(6))
#'
#' df %>%
#'   group_by(g) %>%
#'   summarise(n = n())
#'
#' df %>%
#'   group_by(g) %>%
#'   mutate(across(everything(), ~ paste(current_column(), round(.x, 2))))
#' @name context
NULL

#' @rdname context
#' @export
n <- function() {
  from_context("..group_size")
}

#' @rdname context
#' @export
current_key <- function() {
  peek_mask()$current_key()
}

#' @rdname context
#' @export
current_column <- function() {
  context_env[["..current_column_name"]] %||%
    abort("No current column name registered, current_column() only makes sense inside across()")
}

set_current_column <- function(name) {
  context_env[["..current_column_name"]] <- name
}

poke_current_column <- function(name) {
  old <- context_env[["..current_column_name"]]
  set_current_column(name)
  old
}

context_env <- new_environment()

from_context <- function(what){
  context_env[[what]] %||% abort(glue("{expr} should only be called in a data context", expr = deparse(sys.call(-1))))
}
