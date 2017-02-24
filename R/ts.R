#' @export
filter_.ts <- function(.data, ..., .dots) {
  stop(
    "dplyr::filter() called with ts object. Do you want stats::filter()?",
    call. = FALSE
  )
}
