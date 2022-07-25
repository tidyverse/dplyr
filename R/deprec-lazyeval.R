#' Deprecated SE versions of main verbs.
#'
#' @description
#' `r lifecycle::badge("defunct")`
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

lazy_deprec <- function(fun) {
  lifecycle::deprecate_stop("0.7.0", paste0(fun, "_()"), paste0(fun, "()"))
}

#' @rdname se-deprecated
#' @export
add_count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  lazy_deprec("add_count")
}

#' @rdname se-deprecated
#' @export
add_tally_ <- function(x, wt, sort = FALSE) {
  lazy_deprec("add_tally")
}

#' @export
#' @rdname se-deprecated
arrange_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("arrange")
}

#' @export
#' @rdname se-deprecated
count_ <- function(x, vars, wt = NULL, sort = FALSE, .drop = group_by_drop_default(x)) {
  lazy_deprec("count")
}

#' @export
#' @rdname se-deprecated
#' @inheritParams distinct
distinct_ <- function(.data, ..., .dots, .keep_all = FALSE) {
  lazy_deprec("distinct")
}

#' @export
#' @rdname se-deprecated
do_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("do")
}


#' @export
#' @rdname se-deprecated
filter_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("filter")
}

#' @export
#' @rdname se-deprecated
#' @inheritParams funs
#' @param env The environment in which functions should be evaluated.
funs_ <- function(dots, args = list(), env = base_env()) {
  lazy_deprec("funs")
}

#' @export
#' @rdname se-deprecated
#' @inheritParams group_by
group_by_ <- function(.data, ..., .dots = list(), add = FALSE) {
  lazy_deprec("group_by")
}

#' @export
#' @rdname se-deprecated
group_indices_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("group_indices")
}

#' @export
#' @rdname se-deprecated
mutate_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("mutate")
}

#' @rdname se-deprecated
#' @inheritParams tally
#' @export
tally_ <- function(x, wt, sort = FALSE) {
  lazy_deprec("tally")
}


#' @rdname se-deprecated
#' @export
transmute_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("transmute")
}

#' @rdname se-deprecated
#' @export
rename_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("rename_")
}

#' @export
#' @rdname se-deprecated
rename_vars_ <- function(vars, args) {
  lifecycle::deprecate_stop("0.7.0", "rename_vars_()", "tidyselect::vars_rename()")
}

#' @export
#' @rdname se-deprecated
select_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("select")
}

#' @rdname se-deprecated
#' @param include,exclude Character vector of column names to always
#'   include/exclude.
#' @export
select_vars_ <- function(vars, args, include = chr(), exclude = chr()) {
  lifecycle::deprecate_stop("0.7.0", "select_vars_()", "tidyselect::vars_select()")
}

#' @export
#' @rdname se-deprecated
slice_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("slice")
}

#' @export
#' @rdname se-deprecated
summarise_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("summarise")
}

#' @rdname se-deprecated
#' @export
summarize_ <- function(.data, ..., .dots = list()) {
  lazy_deprec("summarize")
}

#' Summarise and mutate multiple columns.
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `mutate_each()` and `summarise_each()` are deprecated in favour of
#' the new [across()] function that works within `summarise()` and `mutate()`.
#'
#' @keywords internal
#' @export
summarise_each <- function(tbl, funs, ...) {
  lifecycle::deprecate_stop("0.7.0", "summarise_each()", "across()")
}
#' @export
#' @rdname summarise_each
summarise_each_ <- function(tbl, funs, vars) {
  lifecycle::deprecate_stop("0.7.0", "summarise_each_()", "across()")
}

#' @export
#' @rdname summarise_each
mutate_each <- function(tbl, funs, ...) {
  lifecycle::deprecate_warn("0.7.0", "mutate_each()", "across()")
}
#' @export
#' @rdname summarise_each
mutate_each_ <- function(tbl, funs, vars) {
  lifecycle::deprecate_warn("0.7.0", "mutate_each_()", "across()")
}

#' @rdname summarise_each
#' @export
summarize_each <- summarise_each
#' @rdname summarise_each
#' @export
summarize_each_ <- summarise_each_
