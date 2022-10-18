#' Information about the "current" group or variable
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated in dplyr 1.1.0 in favor of [pick()].
#'
#' * `cur_data()` gives the current data for the current group (excluding
#'   grouping variables).
#' * `cur_data_all()` gives the current data for the current group (including
#'   grouping variables).
#'
#' @keywords internal
#' @name deprec-context
NULL

#' @rdname deprec-context
#' @export
cur_data <- function() {
  lifecycle::deprecate_soft(when = "1.1.0", what = "cur_data()", with = "pick()")
  mask <- peek_mask()
  vars <- mask$current_non_group_vars()
  mask$pick_current(vars)
}

#' @rdname deprec-context
#' @export
cur_data_all <- function() {
  lifecycle::deprecate_soft(when = "1.1.0", what = "cur_data_all()", with = "pick()")
  mask <- peek_mask()
  vars <- mask$current_vars()
  mask$pick_current(vars)
}
