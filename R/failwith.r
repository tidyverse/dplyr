#' Fail with specified value.
#'
#' Deprecated. Please use [purrr::possibly()] instead.
#'
#' @param default default value
#' @param f function
#' @param quiet all error messages be suppressed?
#' @return a function
#' @seealso [plyr::try_default()]
#' @keywords internal
#' @export
failwith <- function(default = NULL, f, quiet = FALSE) {
  warning("Deprecated: please use purrr::possibly instead", call. = FALSE)

  function(...) {
    out <- default
    try(out <- f(...), silent = quiet)
    out
  }
}
