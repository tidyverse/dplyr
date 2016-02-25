#' List variables provided by a tbl.
#'
#' @export
#' @param x A tbl object
tbl_vars <- function(x) UseMethod("tbl_vars")
