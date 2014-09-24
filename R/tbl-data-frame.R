# Grouping methods ------------------------------------------------------------

#' @export
regroup.data.frame <- function(x, value) {
  grouped_df(x, value)
}

#' @export
groups.data.frame <- function(x) NULL

#' @export
ungroup.data.frame <- function(x) x

#' @export
group_size.data.frame <- function(x) nrow(x)

#' @export
n_groups.data.frame <- function(x) 1L

# Manipulation functions ------------------------------------------------------

# These could potentially be rewritten to avoid any copies, but since this
# is just a convenience layer, I didn't bother. They should still be fast.

#' @export
filter_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  as.data.frame(filter_(tbl_df(.data), .dots = dots))
}
#' @export
slice_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  as.data.frame(slice_(tbl_df(.data), .dots = dots))
}
#' @export
summarise_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  as.data.frame(summarise_(tbl_df(.data), .dots = dots))
}
#' @export
mutate_.data.frame <-  function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  as.data.frame(mutate_(tbl_df(.data), .dots = dots))
}
#' @export
arrange_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  as.data.frame(arrange_(tbl_df(.data), .dots = dots))
}
#' @export
select_.data.frame <- function(.data, args) {
  args <- lazyeval::as.lazy_dots(args, parent.frame())
  vars <- select_vars_(names(.data), args)
  select_impl(.data, vars)
}
#' @export
rename.data.frame <- function(.data, ...) {
  vars <- rename_vars_(names(.data), lazyeval::lazy_dots(...))
  select_impl(.data, vars)
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

# Set operations ---------------------------------------------------------------

#' @export
intersect.data.frame <- function(x, y, ...) intersect_data_frame(x, y)

#' @export
union.data.frame <-     function(x, y, ...) union_data_frame(x, y)

#' @export
setdiff.data.frame <-   function(x, y, ...) setdiff_data_frame(x, y)

#' @export
setequal.data.frame <-  function(x, y, ...) equal_data_frame(x, y)


# Misc -------------------------------------------------------------------------

#' @export
collect.data.frame <- function(x, ...) x
#' @export
compute.data.frame <- function(x, ...) x
#' @export
collapse.data.frame <- function(x, ...) x
