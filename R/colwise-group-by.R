#' Group by a selection of variables
#'
#' These [scoped] variants of [group_by()] group a data frame by a
#' selection of variables. Like [group_by()], they have optional
#' [mutate] semantics.
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
#'
#' # Group by variables selected by name:
#' group_by_at(mtcars, vars(vs, am))
#'
#' # Like group_by(), the scoped variants have optional mutate
#' # semantics. This provide a shortcut for group_by() + mutate():
#' group_by_all(mtcars, as.factor)
#' group_by_if(iris, is.factor, as.character)
group_by_all <- function(.tbl, .funs = list(), ...) {
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ...)
  if (!length(funs)) {
    funs <- syms(tbl_vars(.tbl))
  }
  group_by(.tbl, !!! funs)
}
#' @rdname group_by_all
#' @export
group_by_at <- function(.tbl, .vars, .funs = list(), ..., .add = FALSE) {
  funs <- manip_at(.tbl, .vars, .funs, enquo(.funs), caller_env(), ...)
  if (!length(funs)) {
    funs <- tbl_at_syms(.tbl, .vars)
  }
  group_by(.tbl, !!! funs, add = .add)
}
#' @rdname group_by_all
#' @export
group_by_if <- function(.tbl, .predicate, .funs = list(), ..., .add = FALSE) {
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
  if (!length(funs)) {
    funs <- tbl_if_syms(.tbl, .predicate)
  }
  group_by(.tbl, !!! funs, add = .add)
}
