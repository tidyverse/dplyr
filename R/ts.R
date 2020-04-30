#' @export
filter.ts <- function(.data, ...) {
  abort(c(
    "Problem with `filter()` input `.data`",
    x = '`.data` is a <ts> object, not a data source',
    i = "Did you want to use `stats::filter()` ?"
  ))
}
