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
#' @param .preserve when `TRUE` (the default), the grouping structure
#'   is preserved, otherwise it is recalculated based on the resulting data.
#' @export
#'
#' @section Grouping variables:
#'
#' The grouping variables that are part of the selection are taken
#' into account to determine filtered rows.
#'
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
filter_all <- function(.tbl, .vars_predicate, .preserve = FALSE) {
  syms <- syms(tbl_vars(.tbl))
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !!pred, .preserve = .preserve)
}
#' @rdname filter_all
#' @export
filter_if <- function(.tbl, .predicate, .vars_predicate, .preserve = FALSE) {
  syms <- tbl_if_syms(.tbl, .predicate, .include_group_vars = TRUE)
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !!pred, .preserve = .preserve)
}
#' @rdname filter_all
#' @export
filter_at <- function(.tbl, .vars, .vars_predicate, .preserve = FALSE) {
  syms <- tbl_at_syms(.tbl, .vars, .include_group_vars = TRUE)
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !!pred, .preserve = .preserve)
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
      "not {friendly_type_of(pred)}"
    )
  }

  pred <- map(syms, function(sym) expr_substitute(pred, quote(.), sym))

  if (length(pred)) {
    joiner(!!!pred)
  } else {
    pred
  }
}
