#' Select variables
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' These functions now live in the tidyselect package as
#' [tidyselect::vars_select()], [tidyselect::vars_rename()] and
#' [tidyselect::vars_pull()].
#'
#' @keywords internal
#' @export
select_vars <- function(vars = chr(), ..., include = chr(), exclude = chr()) {
  lifecycle::deprecate_warn("0.8.4", "select_vars()", "tidyselect::vars_select()")
  tidyselect::vars_select(.vars = vars, ..., .include = include, .exclude = exclude)
}
#' @rdname select_vars
#' @export
rename_vars <- function(vars = chr(), ..., strict = TRUE) {
  lifecycle::deprecate_warn("0.8.4", "rename_vars()", "tidyselect::vars_rename()")
  tidyselect::vars_rename(.vars = vars, ..., .strict = strict)
}
#' @rdname select_vars
#' @export
select_var <- function(vars, var = -1) {
  lifecycle::deprecate_warn("0.8.4", "select_var()", "tidyselect::vars_pull()")
  tidyselect::vars_pull(vars, !!enquo(var))
}
#' @rdname select_vars
#' @export
current_vars <- function(...) {
  lifecycle::deprecate_warn("0.8.4", "current_vars()", "tidyselect::peek_vars()")
  tidyselect::peek_vars(...)
}
