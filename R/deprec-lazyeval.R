#' Defunct SE versions of main verbs.
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
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
#' @name se-defunct
#' @param .data A data frame.
#' @param dots,.dots,... Pair/values of expressions coercible to lazy objects.
#' @param vars Various meanings depending on the verb.
#' @param args Various meanings depending on the verb.
#' @keywords internal
NULL

lazy_defunct <- function(fun) {
  lifecycle::deprecate_stop("0.7.0", paste0(fun, "_()"), paste0(fun, "()"))
}

#' @rdname se-defunct
#' @export
add_count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  lazy_defunct("add_count")
}

#' @rdname se-defunct
#' @export
add_tally_ <- function(x, wt, sort = FALSE) {
  lazy_defunct("add_tally")
}

#' @export
#' @rdname se-defunct
arrange_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("arrange")
}

#' @export
#' @rdname se-defunct
count_ <- function(x, vars, wt = NULL, sort = FALSE, .drop = group_by_drop_default(x)) {
  lazy_defunct("count")
}

#' @export
#' @rdname se-defunct
#' @inheritParams distinct
distinct_ <- function(.data, ..., .dots, .keep_all = FALSE) {
  lazy_defunct("distinct")
}

#' @export
#' @rdname se-defunct
do_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("do")
}

#' @export
#' @rdname se-defunct
filter_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("filter")
}

#' @export
#' @rdname se-defunct
#' @inheritParams funs
#' @param env The environment in which functions should be evaluated.
funs_ <- function(dots, args = list(), env = base_env()) {
  lazy_defunct("funs")
}

#' @export
#' @rdname se-defunct
#' @inheritParams group_by
group_by_ <- function(.data, ..., .dots = list(), add = FALSE) {
  lazy_defunct("group_by")
}

#' @export
#' @rdname se-defunct
group_indices_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("group_indices")
}

#' @export
#' @rdname se-defunct
mutate_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("mutate")
}

#' @rdname se-defunct
#' @inheritParams tally
#' @export
tally_ <- function(x, wt, sort = FALSE) {
  lazy_defunct("tally")
}

#' @rdname se-defunct
#' @export
transmute_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("transmute")
}

#' @rdname se-defunct
#' @export
rename_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("rename")
}

#' @export
#' @rdname se-defunct
rename_vars_ <- function(vars, args) {
  lazy_defunct("rename_vars")
}

#' @export
#' @rdname se-defunct
select_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("select")
}

#' @rdname se-defunct
#' @param include,exclude Character vector of column names to always
#'   include/exclude.
#' @export
select_vars_ <- function(vars, args, include = chr(), exclude = chr()) {
  lazy_defunct("select_vars")
}

#' @export
#' @rdname se-defunct
slice_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("slice")
}

#' @export
#' @rdname se-defunct
summarise_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("summarise")
}

#' @export
#' @rdname se-defunct
summarize_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("summarise")
}

#' Summarise and mutate multiple columns.
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
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
  lazy_defunct("summarise_each")
}

#' @export
#' @rdname summarise_each
mutate_each <- function(tbl, funs, ...) {
  lifecycle::deprecate_stop("0.7.0", "mutate_each()", "across()")
}
#' @export
#' @rdname summarise_each
mutate_each_ <- function(tbl, funs, vars) {
  lazy_defunct("mutate_each")
}

#' @rdname summarise_each
#' @export
summarize_each <- summarise_each
#' @rdname summarise_each
#' @export
summarize_each_ <- summarise_each_
