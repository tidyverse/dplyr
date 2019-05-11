# Grouping methods ------------------------------------------------------------

#' Convert row names to an explicit variable.
#'
#' Deprecated, use [tibble::rownames_to_column()] instead.
#'
#' @param df Input data frame with rownames.
#' @param var Name of variable to use
#' @keywords internal
#' @export
#' @examples
#' mtcars %>% tbl_df()
#'
#' mtcars %>% add_rownames()
add_rownames <- function(df, var = "rowname") {
  warning(
    "Deprecated, use tibble::rownames_to_column() instead.",
    call. = FALSE
  )

  stopifnot(is.data.frame(df))

  rn <- as_tibble(setNames(list(rownames(df)), var))
  rownames(df) <- NULL

  bind_cols(rn, df)
}

# Grouping methods ------------------------------------------------------------

#' @export
group_by.data.frame <- function(.data, ..., add = FALSE, .drop = group_by_drop_default(.data)) {
  groups <- group_by_prepare(.data, ..., add = add)
  grouped_df(groups$data, groups$group_names, .drop)
}
#' @export
group_by_.data.frame <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!!dots, add = add)
}

#' @export
groups.data.frame <- function(x) NULL

#' @export
ungroup.data.frame <- function(x, ...) x

#' @export
group_size.data.frame <- function(x) nrow(x)

#' @export
n_groups.data.frame <- function(x) 1L

# Manipulation functions ------------------------------------------------------

# These could potentially be rewritten to avoid any copies, but since this
# is just a convenience layer, I didn't bother. They should still be fast.

#' @export
filter.data.frame <- function(.data, ..., .preserve = FALSE) {
  as.data.frame(filter(tbl_df(.data), ..., .preserve = .preserve))
}
#' @export
filter_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!!dots)
}

#' @export
slice.data.frame <- function(.data, ..., .preserve = FALSE) {
  as.data.frame(slice(tbl_df(.data), ..., .preserve = .preserve))
}
#' @export
slice_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  slice(.data, !!!dots)
}

#' @export
summarise.data.frame <- function(.data, ...) {
  as.data.frame(summarise(tbl_df(.data), ...))
}
#' @export
summarise_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}

#' @export
mutate.data.frame <- function(.data, ...) {
  as.data.frame(mutate(tbl_df(.data), ...))
}
#' @export
mutate_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  mutate(.data, !!!dots)
}

#' @export
arrange.data.frame <- function(.data, ..., .by_group = FALSE) {
  as.data.frame(arrange(tbl_df(.data), ..., .by_group = .by_group))
}
#' @export
arrange_.data.frame <- function(.data, ..., .dots = list(), .by_group = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  arrange(.data, !!!dots, .by_group = .by_group)
}

#' @export
select.data.frame <- function(.data, ...) {
  # Pass via splicing to avoid matching vars_select() arguments
  vars <- tidyselect::vars_select(sel_vars(.data), !!!enquos(...))
  select_impl(.data, vars)
}
#' @export
select_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  select(.data, !!!dots)
}

#' @export
rename.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_rename(names(.data), !!!enquos(...))
  select_impl(.data, vars)
}
#' @export
rename_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  rename(.data, !!!dots)
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
#' @rdname join.tbl_df
nest_join.data.frame <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ... ) {
  as.data.frame(nest_join(tbl_df(x), y, by = by, copy = copy, ..., keep = keep, name = name))
}

#' @export
right_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(right_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
full_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(full_join(tbl_df(x), y, by = by, copy = copy, ...))
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
intersect.data.frame <- function(x, y, ...) {
  out <- intersect_data_frame(x, y)
  reconstruct_set(out, x)
}

#' @export
union.data.frame <- function(x, y, ...) {
  out <- union_data_frame(x, y)
  reconstruct_set(out, x)
}

#' @export
union_all.data.frame <- function(x, y, ...) {
  out <- bind_rows(x, y)
  reconstruct_set(out, x)
}

#' @export
setdiff.data.frame <- function(x, y, ...) {
  out <- setdiff_data_frame(x, y)
  reconstruct_set(out, x)
}

#' @export
setequal.data.frame <- function(x, y, ...) {
  out <- equal_data_frame(x, y)
  as.logical(out)
}

reconstruct_set <- function(out, x) {
  if (is_grouped_df(x)) {
    out <- grouped_df_impl(out, group_vars(x), group_by_drop_default(x))
  }

  out
}

#' @export
distinct.data.frame <- function(.data, ..., .keep_all = FALSE) {
  dist <- distinct_prepare(.data, enquos(...), .keep_all = .keep_all)
  vars <- match_vars(dist$vars, dist$data)
  keep <- match_vars(dist$keep, dist$data)
  distinct_impl(dist$data, vars, keep, environment())
}
#' @export
distinct_.data.frame <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!!dots, .keep_all = .keep_all)
}


# Do ---------------------------------------------------------------------------

#' @export
do.data.frame <- function(.data, ...) {
  args <- enquos(...)
  named <- named_args(args)

  # Create custom data mask with `.` pronoun
  mask <- new_data_mask(new_environment())
  env_bind_do_pronouns(mask, .data)

  if (!named) {
    out <- eval_tidy(args[[1]], mask)
    if (!inherits(out, "data.frame")) {
      bad("Result must be a data frame, not {fmt_classes(out)}")
    }
  } else {
    out <- map(args, function(arg) list(eval_tidy(arg, mask)))
    names(out) <- names(args)
    out <- tibble::as_tibble(out, validate = FALSE)
  }

  out
}
#' @export
do_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  do(.data, !!!dots)
}

# Random samples ---------------------------------------------------------------


#' @export
sample_n.data.frame <- function(tbl, size, replace = FALSE,
                                weight = NULL, .env = NULL, ...) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  size <- enquo(size)
  weight <- enquo(weight)

  slice(tbl, sample.int(n(), check_size(!!size, n(), replace = replace), replace = replace, prob = !!weight))
}


#' @export
sample_frac.data.frame <- function(tbl, size = 1, replace = FALSE,
                                   weight = NULL, .env = NULL, ...) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  size <- enquo(size)
  weight <- enquo(weight)

  slice(tbl, sample.int(n(), round(n() * check_frac(!!size, replace = replace)), replace = replace, prob = !!weight))
}

# Misc -------------------------------------------------------------------------

#' @export
collect.data.frame <- function(x, ...) x
#' @export
compute.data.frame <- function(x, ...) x
#' @export
collapse.data.frame <- function(x, ...) x
