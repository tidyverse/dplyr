#' Context dependent expressions
#'
#' @description
#' These functions return information about the "current" group or "current"
#' variable, so only work inside specific contexts like `summarise()` and
#' `mutate()`
#'
#' * `n()` gives the current group size.
#' * `cur_data()` gives the current data for the current group (exclusing
#'   grouping variables)
#' * `cur_group()` gives the group keys, a tibble with one row and one column
#'   for each grouping variable.
#' * `cur_group_id()` gives a unique numeric identifier for the current group.
#' * `cur_column()` gives the current column (in [across()] only).
#'
#' See [group_data()] for equivalent functions that return values for all
#' groups.
#'
#' @section data.table:
#' If you're familiar with data.table:
#'
#' * `cur_data()` <-> `.SD`
#' * `cur_group_id()` <-> `.GRP`
#' * `cur_group()` <-> `.BY`
#' * `cur_group_rows()` <-> `.I`
#'
#' @examples
#' df <- tibble(
#'   g = sample(rep(letters[1:3], 1:3)),
#'   x = runif(6),
#'   y = runif(6)
#' )
#' gf <- df %>% group_by(g)
#'
#' gf %>% summarise(n = n())
#'
#' gf %>% mutate(id = cur_group_id())
#' gf %>% summarise(row = cur_group_rows())
#' gf %>% summarise(data = list(cur_group()))
#' gf %>% summarise(data = list(cur_data()))
#'
#' gf %>% mutate(across(everything(), ~ paste(cur_column(), round(.x, 2))))
#' @name context
NULL

#' @rdname context
#' @export
n <- function() {
  length(context_get("..mask")$current_rows())
}

#' @rdname context
#' @export
cur_data <- function() {
  mask <- context_get("..mask")
  data <- mask$full_data()
  mask$pick(setdiff(names(data), group_vars(data)))
}

#' @rdname context
#' @export
cur_group <- function() {
  context_get("..mask")$current_key()
}

#' @rdname context
#' @export
cur_group_id <- function() {
  context_get("..mask")$get_current_group()
}

#' @rdname context
#' @export
cur_group_rows <- function() {
  context_get("..mask")$current_rows()
}

#' @rdname context
#' @export
cur_column <- function() {
  context_env[["..current_column_name"]] %||%
    abort("cur_column() can only be used inside across()")
}

# context accessors -------------------------------------------------------

set_current_column <- function(name) {
  context_env[["..current_column_name"]] <- name
}

poke_current_column <- function(name) {
  old <- context_env[["..current_column_name"]]
  set_current_column(name)
  old
}

context_env <- new_environment()

context_get <- function(key) {
  out <- env_get(context_env, key)
  if (is.null(out)) {
    expr <- deparse(sys.call(-1))
    abort(glue("{expr} should only be called inside a dplyr verb"))
  }
  out
}
