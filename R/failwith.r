#' Fail with specified value.
#'
#' Modify a function so that it returns a default value when there is an
#' error.
#'
#' @param default default value
#' @param f function
#' @param quiet all error messages be suppressed?
#' @return a function
#' @seealso \code{\link[plyr]{try_default}}
#' @keywords debugging
#' @export
#' @examples
#' f <- function(x) if (x == 1) stop("Error!") else 1
#' \dontrun{
#' f(1)
#' f(2)
#' }
#'
#' safef <- failwith(NULL, f)
#' safef(1)
#' safef(2)
failwith <- function(default = NULL, f, quiet = FALSE) {
  function(...) {
    out <- default
    try(out <- f(...), silent = quiet)
    out
  }
}
