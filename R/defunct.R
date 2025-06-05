#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions were deprecated for at least two years before being
#' made defunct. If there's a known replacement, calling the function
#' will tell you about it.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 1.0.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
bench_tbls <- function(tbls, op, ..., times = 10) {
  lifecycle::deprecate_stop("1.0.0", "bench_tbls()")
}

#' @export
#' @rdname defunct
compare_tbls <- function(
  tbls,
  op,
  ref = NULL,
  compare = equal_data_frame,
  ...
) {
  lifecycle::deprecate_stop("1.0.0", "compare_tbls()")
}

#' @export
#' @rdname defunct
compare_tbls2 <- function(
  tbls_x,
  tbls_y,
  op,
  ref = NULL,
  compare = equal_data_frame,
  ...
) {
  lifecycle::deprecate_stop("1.0.0", "compare_tbls2()")
}

#' @export
#' @rdname defunct
eval_tbls <- function(tbls, op) {
  lifecycle::deprecate_stop("1.0.0", "eval_tbls()")
}

#' @export
#' @rdname defunct
eval_tbls2 <- function(tbls_x, tbls_y, op) {
  lifecycle::deprecate_stop("1.0.0", "eval_tbls2()")
}

#' @export
#' @rdname defunct
location <- function(df) {
  lifecycle::deprecate_stop("1.0.0", "location()", "lobstr::ref()")
}

#' @export
#' @rdname defunct
changes <- function(x, y) {
  lifecycle::deprecate_stop("1.0.0", "changes()", "lobstr::ref()")
}
