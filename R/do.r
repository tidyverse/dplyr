#' Apply a function to a tbl
#'
#' This is a general purpose complement to the specialised manipulation
#' functions \code{\link{filter}}, \code{\link{select}}, \code{\link{mutate}},
#' \code{\link{summarise}} and \code{\link{arrange}}.
#'
#' @param .data a tbl
#' @param .f a function to apply. The first unnamed argument supplied to
#'   \code{.f} will be a data frame.
#' @param ... other arguments passed on to the function ()
#' @export
#' @examples
#' by_player <- group_by(baseball, id)
#' by_player <- mutate(by_player, cyear = year - min(year) + 1)
#' system.time(mods <- do(by_player, lm, formula = g ~ poly(cyear, 2)))
#'
#' by_player <- group_by(as.data.table(baseball), id)
#' by_player <- mutate(by_player, cyear = year - min(year) + 1)
#' system.time(mods <- do(by_player, lm, formula = g ~ poly(cyear, 2)))
do <- function(.data, .f, ...) UseMethod("do")

#' @S3method do NULL
do.NULL <- function(.data, .f, ...) {
  NULL
}

#' @S3method do list
do.list <- function(.data, .f, ...) {
  lapply(.data, .f, ...)
}
