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
#' models <- by_cyl %.% do(mod = lm(mpg ~ disp, data = .))
#' models
#'
#' summarise(models, rsq = summary(mod[[1]])$r.squared)
#' models %.% do(data.frame(coef = coef(.$mod[[1]])))
#' models %.% do(data.frame(
#'   var = names(coef(.$mod[[1]])),
#'   coef(summary(.$mod[[1]])))
#' )
#'
#' models <- by_cyl %.% do(
#'   mod_linear = lm(mpg ~ disp, data = .),
#'   mod_quad = lm(mpg ~ poly(disp, 2), data = .)
#' )
#' models
#' compare <- models %.% do(aov = anova(.$mod_linear[[1]], .$mod_quad[[1]]))
#' compare
#' # Want to get to:
#' # compare <- models %.% mutate(aov = anova(mod_linear, .$mod_quad))
#' # compare %.% summarise(p.value = aov$`Pr(>F)`[2])
#'
#' if (require("hflights")) {
#' # You can use it to do any arbitrary computation, like fitting a linear
#' # model. Let's explore how carrier departure delays vary over the time
#' carriers <- group_by(hflights, UniqueCarrier)
#' group_size(carriers)
#'
#' mods <- do(carriers, mod = lm(ArrDelay ~ DepTime, data = .))
#' mods %.% do(as.data.frame(coef(.$mod[[1]])))
#' mods %.% summarise(rsq = summary(mod[[1]])$r.squared)
#'
#' \dontrun{
#' # This longer example shows the progress bar in action
#' by_dest <- hflights %.% group_by(Dest) %.% filter(n() > 100)
#' library(mgcv)
#' by_dest %.% do(smooth = gam(ArrDelay ~ s(DepTime) + Month, data = .))
#' }
#' }
do <- function(.data, ...) UseMethod("do")

#' @export
do.NULL <- function(.data, ...) {
  NULL
}
