#' @export
filter.ts <- function(.data, ...) {
  glubort("{hdr_args(~.data)} must be a data source, got ts object, do you want `stats::filter()`?")
}
