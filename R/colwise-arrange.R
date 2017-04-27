#' Arrange rows by a selection of variables
#'
#' These [scoped] variants of [arrange()] sort a data frame by a
#' selection of variables. Like [arrange()], you can modify the
#' variables before ordering with [funs()].
#'
#' @inheritParams scoped
#' @export
#' @examples
#' df <- as_tibble(mtcars)
#' df
#' arrange_all(df)
#'
#' # You can supply a function that will be applied before taking the
#' # ordering of the variables. The variables of the sorted tibble
#' # keep their original values.
#' arrange_all(df, desc)
#' arrange_all(df, funs(desc(.)))
arrange_all <- function(.tbl, .funs = list(), ...) {
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ...)
  if (!length(funs)) {
    funs <- syms(tbl_vars(.tbl))
  }
  arrange(.tbl, !!! funs)
}
#' @rdname arrange_all
#' @export
arrange_at <- function(.tbl, .vars, .funs = list(), ...) {
  funs <- manip_at(.tbl, .vars, .funs, enquo(.funs), caller_env(), ...)
  if (!length(funs)) {
    funs <- tbl_at_syms(.tbl, .vars)
  }
  arrange(.tbl, !!! funs)
}
#' @rdname arrange_all
#' @export
arrange_if <- function(.tbl, .predicate, .funs = list(), ...) {
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
  if (!length(funs)) {
    funs <- tbl_if_syms(.tbl, .predicate)
  }
  arrange(.tbl, !!! funs)
}
