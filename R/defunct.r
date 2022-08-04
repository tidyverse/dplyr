#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These function were all deprecated for at least two years and are now
#' defunct. Executing them to tell you which function to use instead.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 0.5.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
id <- function(.variables, drop = FALSE) {
  lifecycle::deprecate_stop("0.5.0", "id()", "vctrs::vec_group_id()")
}

#' @usage # Deprecated in 0.7.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
failwith <- function(default = NULL, f, quiet = FALSE) {
  lifecycle::deprecate_stop("0.7.0", "failwith()", "purrr::possibly()")
}

#' @usage # Deprecated in 0.8.* -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
funs <- function(..., .args = list()) {
  lifecycle::deprecate_stop("0.8.0", "funs()", details = paste_line(
    "Please use a list of either functions or lambdas: ",
    "",
    "  # Simple named list: ",
    "  list(mean = mean, median = median)",
    "",
    "  # Auto named with `tibble::lst()`: ",
    "  tibble::lst(mean, median)",
    "",
    "  # Using lambdas",
    "  list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))"
  ))
}

#' @export
#' @rdname defunct
select_vars <- function(vars = chr(), ..., include = chr(), exclude = chr()) {
  lifecycle::deprecate_stop("0.8.4", "select_vars()", "tidyselect::vars_select()")
}
#' @export
#' @rdname defunct
rename_vars <- function(vars = chr(), ..., strict = TRUE) {
  lifecycle::deprecate_stop("0.8.4", "rename_vars()", "tidyselect::vars_rename()")
}
#' @export
#' @rdname defunct
select_var <- function(vars, var = -1) {
  lifecycle::deprecate_stop("0.8.4", "select_var()", "tidyselect::vars_pull()")
}
#' @export
#' @rdname defunct
current_vars <- function(...) {
  lifecycle::deprecate_stop("0.8.4", "current_vars()", "tidyselect::peek_vars()")
}

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
compare_tbls <- function(tbls, op, ref = NULL, compare = equal_data_frame, ...) {
  lifecycle::deprecate_stop("1.0.0", "compare_tbls()")
}

#' @export
#' @rdname defunct
compare_tbls2 <- function(tbls_x, tbls_y, op, ref = NULL, compare = equal_data_frame, ...) {
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
  lifecycle::deprecate_stop("1.0.0", "location()", "lobst::ref()")
}

#' @export
#' @rdname defunct
changes <- function(x, y) {
  lifecycle::deprecate_stop("1.0.0", "changes()", "lobstr::ref()")
}
