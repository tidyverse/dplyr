# Grouping methods ------------------------------------------------------------

#' @export
"groups<-.data.frame" <- function(x, value) {
  grouped_df(x, value, lazy = FALSE)
}

#' @export
groups.data.frame <- function(x) NULL

#' @export
ungroup.data.frame <- function(x) x


# Manipulation functions ------------------------------------------------------

# These could potentially be rewritten to avoid any copies, but since this
# is just a convenience layer, I didn't bother. They should still be fast.

#' @export
filter.data.frame <- function(.data, ...) {
  as.data.frame(filter(tbl_df(.data), ...))  
}
#' @export
summarise.data.frame <- function(.data, ...) {
  as.data.frame(summarise(tbl_df(.data), ...))  
}
#' @export
mutate.data.frame <- function(.data, ...) {
  as.data.frame(mutate(tbl_df(.data), ...))  
}
#' @export
arrange.data.frame <- function(.data, ...) {
  as.data.frame(arrange(tbl_df(.data), ...))  
}
#' @export
select.data.frame <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  input_vars <- vapply(input, as.character, character(1))
  
  .data[, input_vars, drop = FALSE]
}
#' @export
do.data.frame <- function(.data, .f, ...) {
  list(.f(.data, ...))
}

# Joins ------------------------------------------------------------------------

#' @export
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(inner_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(left_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(semi_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(anti_join(tbl_df(x), y, by = by, copy = copy, ...))
}
