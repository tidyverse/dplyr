
#' @export
rows <- function(.data) {
  UseMethod("rows")
}

#' @export
rows.data.frame <- function(.data) {
  rows_impl(.data)
}
