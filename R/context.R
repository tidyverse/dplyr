#' Information about the "current" group or variable
#'
#' @description
#' These functions return information about the "current" group or "current"
#' variable, so only work inside specific contexts like [summarise()] and
#' [mutate()].
#'
#' * `n()` gives the current group size.
#' * `cur_group()` gives the group keys, a tibble with one row and one column
#'   for each grouping variable.
#' * `cur_group_id()` gives a unique numeric identifier for the current group.
#' * `cur_group_rows()` gives the row indices for the current group.
#' * `cur_column()` gives the name of the current column (in [across()] only).
#'
#' See [group_data()] for equivalent functions that return values for all
#' groups.
#'
#' See [pick()] for a way to select a subset of columns using tidyselect syntax
#' while inside `summarise()` or `mutate()`.
#'
#' @section data.table:
#' If you're familiar with data.table:
#'
#' * `cur_group_id()` <-> `.GRP`
#' * `cur_group()` <-> `.BY`
#' * `cur_group_rows()` <-> `.I`
#'
#' See [pick()] for an equivalent to `.SD`.
#'
#' @name context
#' @examples
#' df <- tibble(
#'   g = sample(rep(letters[1:3], 1:3)),
#'   x = runif(6),
#'   y = runif(6)
#' )
#' gf <- df |> group_by(g)
#'
#' gf |> summarise(n = n())
#'
#' gf |> mutate(id = cur_group_id())
#' gf |> reframe(row = cur_group_rows())
#' gf |> summarise(data = list(cur_group()))
#'
#' gf |> mutate(across(everything(), ~ paste(cur_column(), round(.x, 2))))
NULL

#' @rdname context
#' @export
n <- function() {
  peek_mask()$get_current_group_size()
}

#' @rdname context
#' @export
cur_group <- function() {
  peek_mask()$current_key()
}

#' @rdname context
#' @export
cur_group_id <- function() {
  peek_mask()$get_current_group_id()
}

#' @rdname context
#' @export
cur_group_rows <- function() {
  peek_mask()$current_rows()
}

group_labels_details <- function(keys) {
  keys <- map_chr(keys, pillar::format_glimpse)
  labels <- vec_paste0(names(keys), " = ", keys)
  labels <- cli_collapse(labels, last = ", ", sep2 = ", ")
  cli::format_inline("{.code {labels}}")
}

cur_group_label <- function(
  type = mask_type(),
  id = cur_group_id(),
  group = cur_group()
) {
  switch(
    type,
    ungrouped = "",
    grouped = glue("group {id}: {label}", label = group_labels_details(group)),
    rowwise = glue("row {id}"),
    stop_mask_type(type)
  )
}

cur_group_data <- function(mask_type) {
  switch(
    mask_type,
    ungrouped = list(),
    grouped = list(id = cur_group_id(), group = cur_group()),
    rowwise = list(id = cur_group_id()),
    stop_mask_type(mask_type)
  )
}

stop_mask_type <- function(type) {
  cli::cli_abort(
    "Unexpected mask type {.val {type}}.",
    .internal = TRUE
  )
}

cnd_data <- function(cnd, ctxt, mask, call) {
  mask_type <- mask_type(mask)
  has_group_data <- has_active_group_context(mask)

  if (has_group_data) {
    group_data <- cur_group_data(mask_type)
  } else {
    group_data <- NULL
  }

  list(
    cnd = cnd,
    name = ctxt$error_name,
    expr = ctxt$error_expr,
    type = mask_type,
    has_group_data = has_group_data,
    group_data = group_data,
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

  # FIXME: Pass `after = TRUE` once we depend on R 3.5. Currently this
  # doesn't restore in the correct order (FIFO) when context-local
  # functions are called multiple times within the same frame.
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
  context_peek(
    "mask",
    "data-masking verbs like `mutate()`, `filter()`, and `group_by()`",
    call
  )
}
local_mask <- function(x, frame = caller_env()) {
  context_local("mask", x, frame = frame)
}
