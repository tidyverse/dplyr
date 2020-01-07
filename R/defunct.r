#' Defunct functions
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("defunct")}
#' Executing these functions will tell you which function replaces them.
#'
#' @keywords internal
#' @name defunct
NULL

#' @export
#' @rdname defunct
id <- function(.variables, drop = FALSE) {
  lifecycle::deprecate_stop("0.5.0", "id()", "vctrs::vec_group_id()")
}

#' @export
#' @rdname defunct
failwith <- function(default = NULL, f, quiet = FALSE) {
  lifecycle::deprecate_stop("0.7.0", "failwith()", "purrr::possibly()")
}
