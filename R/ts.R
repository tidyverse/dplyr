#' @export
filter.ts <- function(.data, ...) {
  glubort(args = ~.data, "must be a data source, got ts object, do you want `stats::filter()`?")
}
