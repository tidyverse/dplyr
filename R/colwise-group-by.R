#' Group by a selection of variables
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Scoped verbs (`_if`, `_at`, `_all`) have been superseded by the use of
#' [pick()] or [across()] in an existing verb. See `vignette("colwise")` for
#' details.
#'
#' These [scoped] variants of [group_by()] group a data frame by a
#' selection of variables. Like [group_by()], they have optional
#' [mutate] semantics.
#'
#' @inheritParams scoped
#' @inheritParams group_by
#' @param .add See [group_by()]
#'
#' @export
#'
#' @section Grouping variables:
#'
#' Existing grouping variables are maintained, even if not included in
#' the selection.
#'
#' @keywords internal
#' @examples
#' # Group a data frame by all variables:
#' group_by_all(mtcars)
#' # ->
#' mtcars %>% group_by(pick(everything()))
#'
#' # Group by variables selected with a predicate:
#' group_by_if(iris, is.factor)
#' # ->
#' iris %>% group_by(pick(where(is.factor)))
#'
#' # Group by variables selected by name:
#' group_by_at(mtcars, vars(vs, am))
#' # ->
#' mtcars %>% group_by(pick(vs, am))
#'
#' # Like group_by(), the scoped variants have optional mutate
#' # semantics. This provide a shortcut for group_by() + mutate():
#' d <- tibble(x=c(1,1,2,2), y=c(1,2,1,2))
#' group_by_all(d, as.factor)
#' # ->
#' d %>% group_by(across(everything(), as.factor))
#'
#' group_by_if(iris, is.factor, as.character)
#' # ->
#' iris %>% group_by(across(where(is.factor), as.character))
group_by_all <- function(.tbl, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl)) {
  lifecycle::signal_stage("superseded", "group_by_all()")
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ..., .caller = "group_by_all")
  if (!length(funs)) {
    funs <- syms(tbl_vars(.tbl))
  }
  .group_by_static_drop(.tbl, !!!funs, .add = .add, .drop = .drop)
}
#' @rdname group_by_all
#' @export
group_by_at <- function(.tbl, .vars, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl)) {
  lifecycle::signal_stage("superseded", "group_by_at()")
  funs <- manip_at(.tbl, .vars, .funs, enquo(.funs), caller_env(), .include_group_vars = TRUE, ..., .caller = "group_by_at")
  if (!length(funs)) {
    funs <- tbl_at_syms(.tbl, .vars, .include_group_vars = TRUE)
  }
  .group_by_static_drop(.tbl, !!!funs, .add = .add, .drop = .drop)
}
#' @rdname group_by_all
#' @export
group_by_if <- function(.tbl, .predicate, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl)) {
  lifecycle::signal_stage("superseded", "group_by_if()")
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), .include_group_vars = TRUE, ..., .caller = "group_by_if")
  if (!length(funs)) {
    funs <- tbl_if_syms(.tbl, .predicate, .include_group_vars = TRUE)
  }
  .group_by_static_drop(.tbl, !!!funs, .add = .add, .drop = .drop)
}

# workaround so that methods that do not have the .drop argument yet
# don't create the auto mutate .drop column
#
# things like count() and group_by_all()
# can call .group_by_static_drop() instead of group_by()
# so that .drop is only part of the group_by() call if it is FALSE
#
# this is only meant to stay in dplyr until 0.8.0 to give
# implementers of group_by() methods a chance to add .drop in their
# arguments
.group_by_static_drop <- function(..., .drop) {
  if(.drop) {
    group_by(...)
  } else {
    group_by(..., .drop = FALSE)
  }
}
