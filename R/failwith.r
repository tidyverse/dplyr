#' Function operator to add default on error.
#'
#' @export
failwith <- function(default = NULL, f, quiet = FALSE) {
  function(...) {
    out <- default
    try(out <- f(...), silent = quiet)
    out
  }
}
