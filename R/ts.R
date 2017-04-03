#' @export
filter.ts <- function(.data, ...) {
  bad_args(~.data, "must be a data source, not ts object, do you want `stats::filter()`?")
}
