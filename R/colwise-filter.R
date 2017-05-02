#' Filter within a selection of variables
#'
#' These [scoped] filtering verbs apply a predicate expression to a
#' selection of variables. The predicate expression should be quoted
#' with [all_vars()] or [any_vars()] and should mention the pronoun
#' `.` to refer to variables.
#'
#' @inheritParams scoped
#' @param .vars_predicate A quoted predicate expression as returned by
#'   [all_vars()] or [any_vars()].
#' @export
#' @examples
#' # While filter() accepts expressions with specific variables, the
#' # scoped filter verbs take an expression with the pronoun `.` and
#' # replicate it over all variables. This expression should be quoted
#' # with all_vars() or any_vars():
#' all_vars(is.na(.))
#' any_vars(is.na(.))
#'
#'
#' # You can take the intersection of the replicated expressions:
#' filter_all(mtcars, all_vars(. > 150))
#'
#' # Or the union:
#' filter_all(mtcars, any_vars(. > 150))
#'
#'
#' # You can vary the selection of columns on which to apply the
#' # predicate. filter_at() takes a vars() specification:
#' filter_at(mtcars, vars(starts_with("d")), any_vars((. %% 2) == 0))
#'
#' # And filter_if() selects variables with a predicate function:
#' filter_if(mtcars, ~ all(floor(.) == .), all_vars(. != 0))
filter_all <- function(.tbl, .vars_predicate) {
  syms <- syms(tbl_nongroup_vars(.tbl))
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !! pred)
}
#' @rdname filter_all
#' @export
filter_if <- function(.tbl, .predicate, .vars_predicate) {
  syms <- tbl_if_syms(.tbl, .predicate)
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !! pred)
}
#' @rdname filter_all
#' @export
filter_at <- function(.tbl, .vars, .vars_predicate) {
  syms <- tbl_at_syms(.tbl, .vars)
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !! pred)
}

apply_filter_syms <- function(pred, syms, tbl) {
  if (is_empty(syms)) {
    bad_args(".predicate", "has no matching columns")
  }

  if (inherits(pred, "all_vars")) {
    joiner <- all_exprs
  } else if (inherits(pred, "any_vars")) {
    joiner <- any_exprs
  } else {
    bad_args(".vars_predicate", "must be a call to `all_vars()` or `any_vars()`, ",
      "not {type_of(pred)}"
    )
  }

  pred <- map(syms, function(sym) expr_substitute(pred, quote(.), sym))

  if (length(pred)) {
    joiner(!!! pred)
  } else {
    pred
  }
}
