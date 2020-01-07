#' Defunct functions
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("defunct")}
#'
#' @keywords internal
#' @name defunct
NULL

#' @export
#' @rdname defunct
id <- function(.variables, drop = FALSE) {
  lifecycle::deprecate_stop("0.5.0", "id()", "vctrs::vec_group_id()")
}
