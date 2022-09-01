#' Information about "current" group or variable
#'
#' @description
#' These functions return information about the "current" group or "current"
#' variable, so only work inside specific contexts like `summarise()` and
#' `mutate()`
#'
#' * `n()` gives the current group size.
#' * `cur_data()` gives the current data for the current group (excluding
#'   grouping variables).
#' * `cur_data_all()` gives the current data for the current group (including
#'   grouping variables)
#' * `cur_group()` gives the group keys, a tibble with one row and one column
#'   for each grouping variable.
#' * `cur_group_id()` gives a unique numeric identifier for the current group.
#' * `cur_group_rows()` gives the row indices for the current group.
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
#' gf %>% summarise(data = list(cur_data_all()))
#'
#' gf %>% mutate(across(everything(), ~ paste(cur_column(), round(.x, 2))))
#' @name context
NULL

#' @rdname context
#' @export
n <- function() {
  length(peek_mask()$current_rows())
}

#' @rdname context
#' @export
cur_data <- function() {
  mask <- peek_mask()
  vars <- mask$current_non_group_vars()
  mask$pick(vars)
}

#' @rdname context
#' @export
cur_data_all <- function() {
  mask <- peek_mask()
  vars <- mask$current_vars()
  mask$pick(vars)
}

#' @rdname context
#' @export
cur_group <- function() {
  peek_mask()$current_key()
}

#' @rdname context
#' @export
cur_group_id <- function() {
  # [] to get a copy because the current group is dealt with internally
  # if we don't get a copy, code like this won't give correct result:
  # summarise(id = cur_group_id())
  peek_mask()$get_current_group()[]
}

#' @rdname context
#' @export
cur_group_rows <- function() {
  peek_mask()$current_rows()
}

group_labels_details <- function(keys) {
  glue_collapse(map2_chr(keys, names(keys), function(x, name) {
    glue("{name} = {value}", value = pillar::format_glimpse(x))
  }), ", ")
}

cur_group_label <- function(type = mask_type(),
                            id = cur_group_id(),
                            group = cur_group()) {
  switch(
    type,
    ungrouped = "",
    grouped = glue(
      "group {id}: {label}",
      label = group_labels_details(group)
    ),
    rowwise = glue("row {id}"),
    stop_mask_type(type)
  )
}

cur_group_data <- function(mask_type) {
  switch(
    mask_type,
    ungrouped = list(),
    grouped = list(
      id = cur_group_id(),
      group = cur_group()
    ),
    rowwise = list(
      id = cur_group_id()
    ),
    stop_mask_type(mask_type)
  )
}

stop_mask_type <- function(type) {
  cli::cli_abort(
    "Unexpected mask type {.val {type}}.",
    .internal = TRUE
  )
}

cnd_data <- function(cnd, ctxt, mask_type, call) {
  list(
    cnd = cnd,
    name = ctxt$error_name,
    expr = ctxt$error_expression,
    type = mask_type,
    group_data = cur_group_data(mask_type),
    call = call
  )
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
context_peek_bare <- function(name) {
  context_env[[name]]
}
context_peek <- function(name, location, call = caller_env()) {
  context_peek_bare(name) %||%
    abort(glue("Must only be used inside {location}."), call = call)
}
context_local <- function(name, value, frame = caller_env()) {
  old <- context_poke(name, value)
  expr <- expr(on.exit(context_poke(!!name, !!old), add = TRUE))
  eval_bare(expr, frame)
  value
}

peek_column <- function(call = caller_env()) {
  context_peek("column", "`across()`", call)
}
local_column <- function(x, frame = caller_env()) {
  context_local("column", x, frame = frame)
}

peek_mask <- function(call = caller_env()) {
  context_peek("mask", "data-masking verbs like `mutate()`, `filter()`, and `group_by()`", call)
}
local_mask <- function(x, frame = caller_env()) {
  context_local("mask", x, frame = frame)
}
