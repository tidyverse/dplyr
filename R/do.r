#' Apply a function to a tbl
#'
#' This is a general purpose complement to the specialised manipulation
#' functions \code{\link{filter}}, \code{\link{select}}, \code{\link{mutate}},
#' \code{\link{summarise}} and \code{\link{arrange}}. You can use \code{do}
#' to perform arbitrary computation, returning either a data frame or
#' arbitrary objects which will be stored in a list.
#'
#' @param .data a tbl
#' @param ... Expressions to apply to each group. If named, results will be
#'   stored in a new column. If unnamed, should return a data frame. You can
#'   use \code{.} to refer to the current group. You can not mix named and
#'   unnamed arguments.
#' @return
#' \code{do} always returns a data frame. The first columns in the data frame
#' will be the labels, the others will be computed from \code{...}. Named
#' arguments become list-columns, with one row for each group; unnamed
#' elements will be converted to data frames (if needed) and labels will be
#' duplicated accordingly.
#'
#' Unlike \code{\link{summarise}} groups are preserved in input and output.
#' This is because while \code{summarise} reduces the complexity of the
#' data, \code{do} generally does not.
#' @export
#' @examples
#' by_cyl <- group_by(mtcars, cyl)
#' do(by_cyl, head(., 2))
#'
#' models <- do(by_cyl, mod = lm(mpg ~ disp, data = .))
#' do(models, as.data.frame(coef(.$mod[[1]])))
#' do(models, as.data.frame(coef(summary(.$mod[[1]]))))
#' summarise(models, rsq = summary(mod)$r.squared)
#'
#' if (require("hflights")) {
#' by_dest <- group_by(hflights, Dest)
#' do(by_dest, nrow(.))
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
#' mods <- do(carriers, mod = lm(ArrDelay ~ date, data = .))
#' do(mods, coef(.$mod))
#' summarise(mods, rsq = summarise(mod)$r.squared)
#' }
do <- function(.data, ...) UseMethod("do")

#' @export
do.NULL <- function(.data, ...) {
  NULL
}
