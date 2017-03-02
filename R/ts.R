#' @export
filter.ts <- function(.data, ...) {
  abort("dplyr::filter() called with ts object. Do you want stats::filter()?")
}
#' @export
filter_.ts <- function(.data, ..., .dots = list()) {
  filter(.data)
}
