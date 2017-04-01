#' @export
filter.ts <- function(.data, ...) {
  glubort("{hdr_args(~.data)} expected data source, got ts object, do you want `stats::filter()`?")
}
