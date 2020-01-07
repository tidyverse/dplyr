#' Fail with specified value.
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("defunct")}
#' Please use [purrr::possibly()] instead.
#'
#' @param default default value
#' @param f function
#' @param quiet all error messages be suppressed?
#' @return a function
#' @keywords internal
#' @export
failwith <- function(default = NULL, f, quiet = FALSE) {
  lifecycle::deprecate_stop("0.7.0", "failwith()", "purrr::possibly()")
}
