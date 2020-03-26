#' Arrange rows by a selection of variables
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("superseded")}
#'
#' Scoped verbs (`_if`, `_at`, `_all`) have been superseded by the use of
#' [across()] in an existing verb. See `vignette("colwise")` for details.
#'
#' These [scoped] variants of [arrange()] sort a data frame by a
#' selection of variables. Like [arrange()], you can modify the
#' variables before ordering with the `.funs` argument.
#'
#' @inheritParams scoped
#' @inheritParams arrange
#'
#' @section Grouping variables:
#'
#' The grouping variables that are part of the selection participate
#' in the sorting of the data frame.
#'
#' @export
#' @examples
#' df <- as_tibble(mtcars)
#' arrange_all(df)
#' # ->
#' arrange(df, across())
#'
#' arrange_all(df, desc)
#' # ->
#' arrange(df, across(everything(), desc))
arrange_all <- function(.tbl, .funs = list(), ..., .by_group = FALSE) {
  lifecycle::signal_superseded("1.0.0", "arrange_all()", "across()")
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), .include_group_vars = TRUE, ...)
  if (!length(funs)) {
    funs <- syms(tbl_vars(.tbl))
  }
  arrange(.tbl, !!!funs, .by_group = .by_group)
}
#' @rdname arrange_all
#' @export
arrange_at <- function(.tbl, .vars, .funs = list(), ..., .by_group = FALSE) {
  lifecycle::signal_superseded("1.0.0", "arrange_at()", "across()")
  funs <- manip_at(.tbl, .vars, .funs, enquo(.funs), caller_env(), .include_group_vars = TRUE, ...)
  if (!length(funs)) {
    funs <- tbl_at_syms(.tbl, .vars, .include_group_vars = TRUE)
  }
  arrange(.tbl, !!!funs, .by_group = .by_group)
}
#' @rdname arrange_all
#' @export
arrange_if <- function(.tbl, .predicate, .funs = list(), ..., .by_group = FALSE) {
  lifecycle::signal_superseded("1.0.0", "arrange_if()", "across()")
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), .include_group_vars = TRUE, ...)
  if (!length(funs)) {
    funs <- tbl_if_syms(.tbl, .predicate, .include_group_vars = TRUE)
  }
  arrange(.tbl, !!!funs, .by_group = .by_group)
}
