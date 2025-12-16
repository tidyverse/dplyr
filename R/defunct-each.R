#' Defunct functions for working with multiple columns
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `mutate_each()` and `summarise_each()` are deprecated in favour of
#' the new [across()] function that works within `summarise()` and `mutate()`.
#'
#' @name defunct-each
#' @keywords internal
NULL

each_defunct <- function(fun) {
  lifecycle::deprecate_stop(
    when = "0.7.0",
    what = fun,
    with = "across()"
  )
}

#' @export
#' @rdname defunct-each
summarise_each <- function(tbl, funs, ...) {
  each_defunct("summarise_each()")
}
#' @export
#' @rdname defunct-each
summarise_each_ <- function(tbl, funs, vars) {
  each_defunct("summarise_each_()")
}

#' @export
#' @rdname defunct-each
mutate_each <- function(tbl, funs, ...) {
  each_defunct("mutate_each()")
}
#' @export
#' @rdname defunct-each
mutate_each_ <- function(tbl, funs, vars) {
  each_defunct("mutate_each_()")
}

#' @export
#' @rdname defunct-each
summarize_each <- summarise_each
#' @export
#' @rdname defunct-each
summarize_each_ <- summarise_each_
