#' @export
filter.ts <- function(.data, ...) {
  gabort("{hdr_args(~.data)} expected data source, got ts object, do you want `stats::filter()`?")
}
