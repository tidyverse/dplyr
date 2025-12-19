#' Defunct standard evaluation functions
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
#' @name defunct-lazyeval
#' @keywords internal
NULL

lazy_defunct <- function(fun, hint = TRUE) {
  lifecycle::deprecate_stop(
    when = "0.7.0",
    what = paste0(fun, "_()"),
    with = paste0(fun, "()"),
    details = if (hint) "See vignette('programming') for more help"
  )
}

#' @rdname defunct-lazyeval
#' @export
add_count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  lazy_defunct("add_count")
}

#' @rdname defunct-lazyeval
#' @export
add_tally_ <- function(x, wt, sort = FALSE) {
  lazy_defunct("add_tally")
}

#' @export
#' @rdname defunct-lazyeval
arrange_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("arrange")
}

#' @export
#' @rdname defunct-lazyeval
count_ <- function(
  x,
  vars,
  wt = NULL,
  sort = FALSE,
  .drop = group_by_drop_default(x)
) {
  lazy_defunct("count")
}

#' @export
#' @rdname defunct-lazyeval
distinct_ <- function(.data, ..., .dots, .keep_all = FALSE) {
  lazy_defunct("distinct")
}

#' @export
#' @rdname defunct-lazyeval
do_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("do")
}

#' @export
#' @rdname defunct-lazyeval
filter_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("filter")
}

#' @export
#' @rdname defunct-lazyeval
funs_ <- function(dots, args = list(), env = base_env()) {
  lazy_defunct("funs")
}

#' @export
#' @rdname defunct-lazyeval
group_by_ <- function(.data, ..., .dots = list(), add = FALSE) {
  lazy_defunct("group_by")
}

#' @export
#' @rdname defunct-lazyeval
group_indices_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("group_indices", hint = FALSE)
}

#' @export
#' @rdname defunct-lazyeval
mutate_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("mutate")
}

#' @rdname defunct-lazyeval
#' @export
tally_ <- function(x, wt, sort = FALSE) {
  lazy_defunct("tally")
}

#' @rdname defunct-lazyeval
#' @export
transmute_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("transmute")
}

#' @rdname defunct-lazyeval
#' @export
rename_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("rename", hint = FALSE)
}

#' @export
#' @rdname defunct-lazyeval
select_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("select", hint = FALSE)
}

#' @export
#' @rdname defunct-lazyeval
slice_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("slice", hint = FALSE)
}

#' @export
#' @rdname defunct-lazyeval
summarise_ <- function(.data, ..., .dots = list()) {
  lazy_defunct("summarise", hint = FALSE)
}

#' @rdname defunct-lazyeval
#' @export
summarize_ <- summarise_
