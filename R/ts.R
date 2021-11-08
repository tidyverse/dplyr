#' @export
filter.ts <- function(.data, ...) {
  bullets <- c(
    "Incompatible data source.",
    x = '`.data` is a <ts> object, not a data source.',
    i = "Did you want to use `stats::filter()`?"
  )
  abort(bullets)
}
