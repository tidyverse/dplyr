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
    call. = FALSE)

  stopifnot(is.data.frame(df))

  rn <- as_data_frame(setNames(list(rownames(df)), var))
  rownames(df) <- NULL

  bind_cols(rn, df)
}

# Grouping methods ------------------------------------------------------------

#' @export
group_by.data.frame <- function(.data, ..., add = FALSE) {
  groups <- group_by_prepare(.data, ..., add = add)
  grouped_df(groups$data, groups$group_names)
}
#' @export
group_by_.data.frame <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!! dots, add = add)
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
filter.data.frame <- function(.data, ...) {
  as.data.frame(filter(tbl_df(.data), ...))
}
#' @export
filter_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!! dots)
}

#' @export
slice.data.frame <- function(.data, ...) {
  dots <- named_quos(...)
  slice_impl(.data, dots)
}
#' @export
slice_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  slice_impl(.data, dots)
}

#' @export
summarise.data.frame <- function(.data, ...) {
  as.data.frame(summarise(tbl_df(.data), ...))
}
#' @export
summarise_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!! dots)
}

#' @export
mutate.data.frame <- function(.data, ...) {
  as.data.frame(mutate(tbl_df(.data), ...))
}
#' @export
mutate_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  mutate(.data, !!! dots)
}

#' @export
arrange.data.frame <- function(.data, ...) {
  as.data.frame(arrange(tbl_df(.data), ...))
}
#' @export
arrange_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  arrange(.data, !!! dots)
}

#' @export
select.data.frame <- function(.data, ...) {
  # Pass via splicing to avoid matching select_vars() arguments
  vars <- select_vars(names(.data), !!! quos(...))
  select_impl(.data, vars)
}
#' @export
select_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  select(.data, !!! dots)
}

#' @export
rename.data.frame <- function(.data, ...) {
  vars <- rename_vars(names(.data), !!! quos(...))
  select_impl(.data, vars)
}
#' @export
rename_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  rename(.data, !!! dots)
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
intersect.data.frame <- function(x, y, ...) intersect_data_frame(x, y)

#' @export
union.data.frame <-     function(x, y, ...) union_data_frame(x, y)

#' @export
union_all.data.frame <- function(x, y, ...) bind_rows(x, y)

#' @export
setdiff.data.frame <-   function(x, y, ...) setdiff_data_frame(x, y)

#' @export
setequal.data.frame <-  function(x, y, ...) equal_data_frame(x, y)

#' @export
distinct.data.frame <- function(.data, ..., .keep_all = FALSE) {
  dist <- distinct_vars(.data, named_quos(...), .keep_all = .keep_all)
  distinct_impl(dist$data, dist$vars, dist$keep)
}
#' @export
distinct_.data.frame <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!! dots, .keep_all = .keep_all)
}


# Do ---------------------------------------------------------------------------

#' @export
do.data.frame <- function(.data, ...) {
  args <- quos(...)
  named <- named_args(args)

  # Create custom dynamic scope with `.` pronoun
  # FIXME: Pass without splicing once child_env() calls env_bind()
  # with explicit arguments
  overscope <- child_env(NULL, !!! list(. = .data, .data = .data))

  if (!named) {
    out <- eval_tidy_(args[[1]], overscope)
    if (!inherits(out, "data.frame")) {
      bad("Result must be a data frame, not {fmt_classes(out)}")
    }
  } else {
    out <- map(args, function(arg) list(eval_tidy_(arg, overscope)))
    names(out) <- names(args)
    out <- tibble::as_tibble(out, validate = FALSE)
  }

  out
}
#' @export
do_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  do(.data, !!! dots)
}

# Random samples ---------------------------------------------------------------


#' @export
sample_n.data.frame <- function(tbl, size, replace = FALSE,
                                weight = NULL, .env = NULL) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  weight <- eval_tidy(enquo(weight), tbl)
  sample_n_basic(tbl, size, FALSE, replace = replace, weight = weight)
}


#' @export
sample_frac.data.frame <- function(tbl, size = 1, replace = FALSE,
                                   weight = NULL, .env = NULL) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  weight <- eval_tidy(enquo(weight), tbl)
  sample_n_basic(tbl, size, TRUE, replace = replace, weight = weight)
}

sample_n_basic <- function(tbl, size, frac, replace = FALSE, weight = NULL) {
  n <- nrow(tbl)

  weight <- check_weight(weight, n)
  assert_that(is.numeric(size), length(size) == 1, size >= 0)

  if (frac) {
    check_frac(size, replace)
    size <- round(size * n)
  } else {
    check_size(size, n, replace)
  }

  idx <- sample.int(n, size, replace = replace, prob = weight)
  tbl[idx, , drop = FALSE]
}



# Misc -------------------------------------------------------------------------

#' @export
collect.data.frame <- function(x, ...) x
#' @export
compute.data.frame <- function(x, ...) x
#' @export
collapse.data.frame <- function(x, ...) x
