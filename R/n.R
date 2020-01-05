#' The number of observations in the current group.
#'
#' This function is implemented specifically for each data source and can only
#' be used from within [summarise()], [mutate()] and
#' [filter()].
#'
#' @export
#' @examples
#' if (require("nycflights13")) {
#' carriers <- group_by(flights, carrier)
#' summarise(carriers, n())
#' mutate(carriers, n = n())
#' filter(carriers, n() < 100)
#' }
n <- function() {
  from_context("..group_size")
}
