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
#' * `cur_column()` gives the name of the current column (in [across()] only).
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
  length(peek_mask("n()")$current_rows())
}

#' @rdname context
#' @export
cur_data <- function() {
  mask <- peek_mask("cur_data()")
  vars <- mask$current_non_group_vars()
  mask$pick(vars)
}

#' @rdname context
#' @export
cur_group <- function() {
  peek_mask("cur_group()")$current_key()
}

#' @rdname context
#' @export
cur_group_id <- function() {
  peek_mask("cur_group_id()")$get_current_group()
}

#' @rdname context
#' @export
cur_group_rows <- function() {
  peek_mask("cur_group_rows()")$current_rows()
}

group_labels_details <- function(keys) {
  glue_collapse(map2_chr(keys, names(keys), function(x, name) {
    glue("{name} = {value}", value = format_v(x))
  }), ", ")
}

cur_group_label <- function() {
  mask <- peek_mask("cur_group_label()")
  data <- mask$full_data()
  if (is_grouped_df(data)) {
    paste0("group ", cur_group_id(), ": ", group_labels_details(cur_group()))
  } else if (inherits(data, "rowwise_df")) {
    paste0("row ", cur_group_id())
  } else {
    ""
  }
}

#' @rdname context
#' @export
cur_column <- function() {
  peek_column()
}

# context accessors -------------------------------------------------------

context_env <- new_environment()
context_poke <- function(name, value) {
  old <- context_env[[name]]
  context_env[[name]] <- value
  old
}
context_peek <- function(name, fun, location = "dplyr verbs") {
  context_env[[name]] %||%
    abort(glue("{fun} must only be used inside {location}"))
}
context_local <- function(name, value, frame = caller_env()) {
  old <- context_poke(name, value)
  expr <- expr(on.exit(context_poke(!!name, !!old), add = TRUE))
  eval_bare(expr, frame)
}

peek_column <- function() {
  context_peek("column", "cur_column()", "across()")
}
local_column <- function(x, frame = caller_env()) {
  context_local("column", x, frame = frame)
}

peek_mask <- function(fun = "peek_mask()") {
  context_peek("mask", fun)
}
local_mask <- function(x, frame = caller_env()) {
  context_local("mask", x, frame = frame)
}
