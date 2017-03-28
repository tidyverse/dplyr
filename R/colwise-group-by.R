#' Group by a selection of variables.
#'
#' @inheritParams scoped
#' @param .add Passed to the `add` argument of [group_by()].
#' @export
#' @examples
#' # Group a data frame by all variables:
#' group_by_all(mtcars)
#'
#' # Group by variables selected with a predicate:
#' group_by_if(iris, is.factor)
#' group_by_if(mtcars, rlang::is_integerish)
#'
#' # Group by variables selected by name:
#' group_by_at(mtcars, vars(vs, am))
group_by_all <- function(.tbl) {
  syms <- syms(tbl_vars(.tbl))
  group_by(.tbl, !!! syms(syms))
}
#' @rdname group_by_all
#' @export
group_by_at <- function(.tbl, .vars, .add = FALSE) {
  syms <- tbl_at_syms(.tbl, .vars)
  group_by(.tbl, !!! syms, add = .add)
}
#' @rdname group_by_all
#' @export
group_by_if <- function(.tbl, .predicate, .add = FALSE) {
  syms <- tbl_if_syms(.tbl, .predicate)
  group_by(.tbl, !!! syms, add = .add)
}
