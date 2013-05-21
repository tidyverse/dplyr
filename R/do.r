#' Apply a function to a data source
#'
#' This is a general purpose complement to the specialised manipulation
#' functions \code{\link{filter}}, \code{\link{select}}, \code{\link{mutate}},
#' \code{\link{summarise}} and \code{\link{arrange}}.
#'
#' @param .data a data source
#' @param .f a function to apply. The first unnamed argument supplied to
#'   \code{.f} will be a data frame.
#' @param ... other arguments passed on to the function ()
#' @export
do <- function(.data, .f, ...) UseMethod("do")

#' @S3method do NULL
do.NULL <- function(.data, .f, ...) {
  NULL
}

#' @S3method do list
do.list <- function(.data, .f, ...) {
  lapply(.data, .f, ...)
}
