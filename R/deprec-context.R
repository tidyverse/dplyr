#' Information about the "current" group or variable
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated in dplyr 1.1.0.
#'
#' * `cur_data()` is deprecated in favor of [pick()].
#' * `cur_data_all()` is deprecated but does not have a direct replacement as
#'   selecting the grouping variables is not well-defined and is unlikely to
#'   ever be useful.
#'
#' @keywords internal
#' @name deprec-context
NULL

#' @rdname deprec-context
#' @export
cur_data <- function() {
  lifecycle::deprecate_warn(
    when = "1.1.0",
    what = "cur_data()",
    with = "pick()",
    id = "dplyr-cur-data"
  )
  mask <- peek_mask()
  vars <- mask$current_non_group_vars()
  mask$pick_current(vars)
}

#' @rdname deprec-context
#' @export
cur_data_all <- function() {
  lifecycle::deprecate_warn(
    when = "1.1.0",
    what = "cur_data_all()",
    with = "pick()",
    id = "dplyr-cur-data-all"
  )
  mask <- peek_mask()
  vars <- mask$current_vars()
  mask$pick_current(vars)
}
