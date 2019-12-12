#' Deprecated SE versions of main verbs.
#'
#' dplyr used to offer twin versions of each verb suffixed with an
#' underscore. These versions had standard evaluation (SE) semantics:
#' rather than taking arguments by code, like NSE verbs, they took
#' arguments by value. Their purpose was to make it possible to
#' program with dplyr. However, dplyr now uses tidy evaluation
#' semantics. NSE verbs still capture their arguments, but you can now
#' unquote parts of these arguments. This offers full programmability
#' with NSE verbs. Thus, the underscored versions are now superfluous.
#'
#' Unquoting triggers immediate evaluation of its operand and inlines
#' the result within the captured expression. This result can be a
#' value or an expression to be evaluated later with the rest of the
#' argument. See `vignette("programming")` for more information.
#'
#' @name se-deprecated
#' @param .data A data frame.
#' @param dots,.dots,... Pair/values of expressions coercible to lazy objects.
#' @param vars Various meanings depending on the verb.
#' @param args Various meanings depending on the verb.
#' @keywords internal
NULL

# -------------------------------------------------------------------------

#' @rdname se-deprecated
#' @export
add_count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  signal_soft_deprecated(paste_line(
    "add_count_() is deprecated. ",
    "Please use add_count() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with add_count() : https://tidyeval.tidyverse.org"
  ))

  vars <- compat_lazy_dots(vars, caller_env())
  wt <- wt %||% quo(NULL)
  wt <- compat_lazy(wt, caller_env())
  add_count(x, !!!vars, wt = !!wt, sort = sort)
}

#' @rdname se-deprecated
#' @export
add_tally_ <- function(x, wt, sort = FALSE) {
  signal_soft_deprecated(paste_line(
    "add_tally_() is deprecated. ",
    "Please use add_tally() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with add_tally() : https://tidyeval.tidyverse.org"
  ))

  wt <- compat_lazy(wt, caller_env())
  add_tally(x, !!wt, sort = sort)
}

#' @export
arrange.default <- function(.data, ...) {
  arrange_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
arrange_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "arrange_() is deprecated. ",
    "Please use arrange() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with arrange() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("arrange_")
}
#' @export
arrange_.data.frame <- function(.data, ..., .dots = list(), .by_group = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  arrange(.data, !!!dots, .by_group = .by_group)
}
#' @export
arrange_.tbl_df <- function(.data, ..., .dots = list(), .by_group = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  arrange_data_frame(.data, !!!dots, .by_group = .by_group)
}


#' @export
#' @rdname se-deprecated
count_ <- function(x, vars, wt = NULL, sort = FALSE, .drop = group_by_drop_default(x)) {
  signal_soft_deprecated(paste_line(
    "count_() is deprecated. ",
    "Please use count() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with count() : https://tidyeval.tidyverse.org"
  ))

  vars <- compat_lazy_dots(vars, caller_env())
  wt <- wt %||% quo(NULL)
  wt <- compat_lazy(wt, caller_env())
  count(x, !!!vars, wt = !!wt, sort = sort, .drop = .drop)
}


#' @export
distinct.default <- function(.data, ..., .keep_all = FALSE) {
  distinct_(.data, .dots = compat_as_lazy_dots(...), .keep_all = .keep_all)
}
#' @export
#' @rdname se-deprecated
#' @inheritParams distinct
distinct_ <- function(.data, ..., .dots, .keep_all = FALSE) {
  signal_soft_deprecated(paste_line(
    "distinct_() is deprecated. ",
    "Please use distinct() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with distinct() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("distinct_")
}
#' @export
distinct_.data.frame <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!!dots, .keep_all = .keep_all)
}
#' @export
distinct_.grouped_df <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!!dots, .keep_all = .keep_all)
}

#' @export
#' @rdname se-deprecated
do_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "do_() is deprecated. ",
    "Please use group_map() instead"
  ))
  UseMethod("do_")
}
#' @export
do.default <- function(.data, ...) {
  do_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
do_.NULL <- function(.data, ..., .dots = list()) {
  NULL
}
#' @export
do_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  do(.data, !!!dots)
}
#' @export
do_.grouped_df <- function(.data, ..., env = caller_env(), .dots = list()) {
  dots <- compat_lazy_dots(.dots, env, ...)
  do(.data, !!!dots)
}
#' @export
do_.rowwise_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  do(.data, !!!dots)
}


#' @export
filter.default <- function(.data, ..., .preserve = FALSE) {
  filter_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
filter_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "filter_() is deprecated. ",
    "Please use filter() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with filter() : https://tidyeval.tidyverse.org"
  ))
  UseMethod("filter_")
}
#' @export
filter_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!!dots)
}
#' @export
filter_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!!dots)
}

#' @export
#' @rdname se-deprecated
#' @inheritParams funs
#' @param env The environment in which functions should be evaluated.
funs_ <- function(dots, args = list(), env = base_env()) {
  signal_soft_deprecated(paste_line(
    "funs_() is deprecated. ",
    "Please use list() instead"
  ))

  dots <- compat_lazy_dots(dots, caller_env())
  funs(!!!dots, .args = args)
}

#' @export
group_by.default <- function(.data, ..., add = FALSE, .drop = group_by_drop_default(.data)) {
  group_by_(.data, .dots = compat_as_lazy_dots(...), add = add)
}
#' @export
#' @rdname se-deprecated
#' @inheritParams group_by
group_by_ <- function(.data, ..., .dots = list(), add = FALSE) {
  signal_soft_deprecated(paste_line(
    "group_by_() is deprecated. ",
    "Please use group_by() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with group_by() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("group_by_")
}
#' @export
group_by_.data.frame <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!!dots, add = add)
}
#' @export
group_by_.rowwise_df <- function(.data, ..., .dots = list(), add = FALSE, .drop = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!!dots, add = add, .drop = .drop)
}


#' @export
#' @rdname se-deprecated
group_indices_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "group_indices_() is deprecated. ",
    "Please use group_indices() instead"
  ))

  UseMethod("group_indices_")
}
#' @export
group_indices.data.frame <- function(.data, ..., .drop = TRUE) {
  dots <- enquos(...)
  if (length(dots) == 0L) {
    return(rep(1L, nrow(.data)))
  }
  group_indices(group_by(.data, !!!dots, .drop = .drop))
}
#' @export
group_indices_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_indices(.data, !!!dots)
}
#' @export
group_indices_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_indices(.data, !!!dots)
}
#' @export
group_indices_.rowwise_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_indices(.data, !!!dots)
}

#' @export
mutate.default <- function(.data, ...) {
  mutate_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
mutate_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "mutate_() is deprecated. ",
    "Please use mutate() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with mutate() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("mutate_")
}
#' @export
mutate_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  mutate(.data, !!!dots)
}
#' @export
mutate_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  mutate(.data, !!!dots)
}

#' @rdname se-deprecated
#' @inheritParams tally
#' @export
tally_ <- function(x, wt, sort = FALSE) {
  signal_soft_deprecated(paste_line(
    "tally_() is deprecated. ",
    "Please use tally() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with tally() : https://tidyeval.tidyverse.org"
  ))

  wt <- compat_lazy(wt, caller_env())
  tally(x, wt = !!wt, sort = sort)
}


#' @rdname se-deprecated
#' @export
transmute_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "transmute_() is deprecated. ",
    "Please use transmute() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with transmute() : https://tidyeval.tidyverse.org"
  ))
  UseMethod("transmute_")
}
#' @export
transmute_.default <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  transmute(.data, !!!dots)
}
#' @export
transmute_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  transmute(.data, !!!dots)
}

#' @export
rename.default <- function(.data, ...) {
  rename_(.data, .dots = compat_as_lazy_dots(...))
}
#' @rdname se-deprecated
#' @export
rename_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "rename_() is deprecated. ",
    "Please use rename() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with rename() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("rename_")
}
#' @export
rename_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  rename(.data, !!!dots)
}
#' @export
rename_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  rename(.data, !!!dots)
}


#' @export
#' @rdname se-deprecated
rename_vars_ <- function(vars, args) {
  warn_deprecated(paste_line(
    "rename_vars_() is deprecated. ",
    "Please use tidyselect::vars_rename() instead"
  ))
  args <- compat_lazy_dots(args, caller_env())
  rename_vars(vars, !!!args)
}

#' @export
select.default <- function(.data, ...) {
  select_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
select_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "select_() is deprecated. ",
    "Please use select() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with select() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("select_")
}
#' @export
select_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  select(.data, !!!dots)
}
#' @export
select_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  select.grouped_df(.data, !!!dots)
}


#' @rdname se-deprecated
#' @param include,exclude Character vector of column names to always
#'   include/exclude.
#' @export
select_vars_ <- function(vars, args, include = chr(), exclude = chr()) {
  warn_deprecated(paste_line(
    "select_vars_() is deprecated. ",
    "Please use tidyselect::vars_select() instead"
  ))

  args <- compat_lazy_dots(args, caller_env())
  select_vars(vars, !!!args, include = include, exclude = exclude)
}

#' @export
slice.default <- function(.data, ..., .preserve = FALSE) {
  slice_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
slice_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "slice_() is deprecated. ",
    "Please use slice() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with slice() : https://tidyeval.tidyverse.org"
  ))
  UseMethod("slice_")
}
#' @export
slice_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  slice(.data, !!!dots)
}
#' @export
slice_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  slice(.data, !!!dots)
}


#' @export
summarise.default <- function(.data, ...) {
  summarise_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
summarise_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "summarise_() is deprecated. ",
    "Please use summarise() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with summarise() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("summarise_")
}
#' @export
summarise_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}
#' @export
summarise_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  summarise(.data, !!!dots)
}
#' @rdname se-deprecated
#' @export
summarize_ <- summarise_


#' Summarise and mutate multiple columns.
#'
#' \Sexpr[results=rd, stage=render]{dplyr:::lifecycle("deprecated")}
#'
#' @description
#'
#' `mutate_each()` and `summarise_each()` are deprecated in favour of
#' a more featureful family of functions: [mutate_all()],
#' [mutate_at()], [mutate_if()], [summarise_all()], [summarise_at()]
#' and [summarise_if()].
#'
#' The `_each()` functions have two replacements depending on what
#' variables you want to apply `funs` to. To apply a function to all
#' variables, use [mutate_all()] or [summarise_all()]. To apply a
#' function to a selection of variables, use [mutate_at()] or
#' [summarise_at()].
#'
#' See the relevant section of `vignette("compatibility")` for more
#' information.
#'
#' @keywords internal
#' @export
summarise_each <- function(tbl, funs, ...) {
  summarise_each_(tbl, funs, enquos(...))
}
#' @export
#' @rdname summarise_each
summarise_each_ <- function(tbl, funs, vars) {
  signal_soft_deprecated(paste_line(
    "summarise_each() is deprecated",
    "Please use summarise_if(), summarise_at(), or summarise_all() instead: ",
    "",
    "  - To map `funs` over all variables, use summarise_all()",
    "  - To map `funs` over a selection of variables, use summarise_at()"
  ))

  if (is_empty(vars)) {
    vars <- tbl_nongroup_vars(tbl)
  } else {
    vars <- compat_lazy_dots(vars, caller_env())
    vars <- tidyselect::vars_select(tbl_nongroup_vars(tbl), !!!vars)
    if (length(vars) == 1 && names(vars) == as_string(vars)) {
      vars <- unname(vars)
    }
  }
  if (is_character(funs)) {
    funs <- funs_(funs)
  }
  funs <- manip_at(tbl, vars, funs, enquo(funs), caller_env())
  summarise(tbl, !!!funs)
}

#' @export
#' @rdname summarise_each
mutate_each <- function(tbl, funs, ...) {
  if (is_character(funs)) {
    funs <- funs_(funs)
  }

  mutate_each_(tbl, funs, enquos(...))
}
#' @export
#' @rdname summarise_each
mutate_each_ <- function(tbl, funs, vars) {
  signal_soft_deprecated(paste_line(
    "mutate_each() is deprecated",
    "Please use mutate_if(), mutate_at(), or mutate_all() instead: ",
    "",
    "  - To map `funs` over all variables, use mutate_all()",
    "  - To map `funs` over a selection of variables, use mutate_at()"
  ))
  if (is_empty(vars)) {
    vars <- tbl_nongroup_vars(tbl)
  } else {
    vars <- compat_lazy_dots(vars, caller_env())
    vars <- tidyselect::vars_select(tbl_nongroup_vars(tbl), !!!vars)
    if (length(vars) == 1 && names(vars) == as_string(vars)) {
      vars <- unname(vars)
    }
  }
  funs <- manip_at(tbl, vars, funs, enquo(funs), caller_env())
  mutate(tbl, !!!funs)
}

#' @rdname summarise_each
#' @export
summarize_each <- summarise_each
#' @rdname summarise_each
#' @export
summarize_each_ <- summarise_each_
