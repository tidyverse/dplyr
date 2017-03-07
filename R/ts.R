#' @export
filter.ts <- function(.data, ...) {
  abort("dplyr::filter() called with ts object. Do you want stats::filter()?")
}
