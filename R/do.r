#' Apply a function to a tbl
#'
#' This is a general purpose complement to the specialised manipulation
#' functions \code{\link{filter}}, \code{\link{select}}, \code{\link{mutate}},
#' \code{\link{summarise}} and \code{\link{arrange}}.
#'
#' @param .data a tbl
#' @param .f a function to apply to each piece. The first unnamed argument
#'   supplied to \code{.f} will be a data frame.
#' @param ... other arguments passed on to the function ()
#' @export
#' @examples
#' if (require("hflights")) {
#' by_dest <- group_by(hflights, Dest)
#' do(by_dest, nrow)
#' # Inefficient version of
#' group_size(by_dest)
#'
#' # You can use it to do any arbitrary computation, like fitting a linear
#' # model. Let's explore how carrier departure delays vary over the course
#' # of a year
#' jan <- filter(hflights, Month == 1)
#' jan <- mutate(jan, date = ISOdate(Year, Month, DayofMonth))
#' carriers <- group_by(hflights, UniqueCarrier)
#' group_size(carriers)
#'
#' mods <- do(carriers, failwith(NULL, lm), formula = ArrDelay ~ date)
#' sapply(mods, coef)
#' }
do <- function(.data, .f, ...) UseMethod("do")

#' @export
do.NULL <- function(.data, .f, ...) {
  NULL
}

#' @export
do.list <- function(.data, .f, ...) {
  lapply(.data, .f, ...)
}
