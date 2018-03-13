#' @export
filter.ts <- function(.data, ...) {
  bad_args(".data", "must be a data source, not a ts object, do you want `stats::filter()`?")
}
