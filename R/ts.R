#' @export
filter.ts <- function(.data, ...) {
  glubort(args = ~.data, "must be a data source, not ts object, do you want `stats::filter()`?")
}
