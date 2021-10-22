#' Filter within a selection of variables
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Scoped verbs (`_if`, `_at`, `_all`) have been superseded by the use of
#' [across()] in an existing verb. See `vignette("colwise")` for details.
#'
#' These [scoped] filtering verbs apply a predicate expression to a
#' selection of variables. The predicate expression should be quoted
#' with [all_vars()] or [any_vars()] and should mention the pronoun
#' `.` to refer to variables.
#'
#' @inheritParams scoped
#' @param .vars_predicate A quoted predicate expression as returned by
#'   [all_vars()] or [any_vars()].
#'
#'   Can also be a function or purrr-like formula. In this case, the
#'   intersection of the results is taken by default and there's
#'   currently no way to request the union.
#' @param .preserve when `FALSE` (the default), the grouping structure
#'   is recalculated based on the resulting data, otherwise it is kept as is.
#' @export
#'
#' @section Grouping variables:
#'
#' The grouping variables that are part of the selection are taken
#' into account to determine filtered rows.
#'
#' @keywords internal
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
#' # ->
#' filter(mtcars, if_all(everything(), ~ .x > 150))
#'
#' # Or the union:
#' filter_all(mtcars, any_vars(. > 150))
#' # ->
#' filter(mtcars, if_any(everything(), ~ . > 150))
#'
#'
#' # You can vary the selection of columns on which to apply the
#' # predicate. filter_at() takes a vars() specification:
#' filter_at(mtcars, vars(starts_with("d")), any_vars((. %% 2) == 0))
#' # ->
#' filter(mtcars, if_any(starts_with("d"), ~ (.x %% 2) == 0))
#'
#' # And filter_if() selects variables with a predicate function:
#' filter_if(mtcars, ~ all(floor(.) == .), all_vars(. != 0))
#' # ->
#' is_int <- function(x) all(floor(x) == x)
#' filter(mtcars, if_all(where(is_int), ~ .x != 0))
filter_all <- function(.tbl, .vars_predicate, .preserve = FALSE) {
  lifecycle::signal_stage("superseded", "filter_all()")
  syms <- syms(tbl_vars(.tbl))
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !!pred, .preserve = .preserve)
}
#' @rdname filter_all
#' @export
filter_if <- function(.tbl, .predicate, .vars_predicate, .preserve = FALSE) {
  lifecycle::signal_stage("superseded", "filter_if()")
  syms <- tbl_if_syms(.tbl, .predicate, .include_group_vars = TRUE)
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !!pred, .preserve = .preserve)
}
#' @rdname filter_all
#' @export
filter_at <- function(.tbl, .vars, .vars_predicate, .preserve = FALSE) {
  lifecycle::signal_stage("superseded", "filter_at()")
  syms <- tbl_at_syms(.tbl, .vars, .include_group_vars = TRUE)
  pred <- apply_filter_syms(.vars_predicate, syms, .tbl)
  filter(.tbl, !!pred, .preserve = .preserve)
}

apply_filter_syms <- function(pred, syms, tbl, error_call = caller_env()) {
  if (is_empty(syms)) {
    msg  <- glue("`.predicate` must match at least one column.")
    abort(msg, call = error_call)
  }
  joiner <- all_exprs

  if (inherits_any(pred, c("all_vars", "any_vars"))) {
    if (inherits(pred, "any_vars")) {
      joiner <- any_exprs
    }
    pred <- map(syms, function(sym) expr_substitute(pred, quote(.), sym))
  } else if (is_bare_formula(pred) || is_function(pred)) {
    pred <- as_function(pred)
    pred <- map(syms, function(sym) call2(pred, sym))
  } else {
    msg <- glue("`.vars_predicate` must be a function or a call to `all_vars()` or `any_vars()`, not {friendly_type_of(pred)}.")
    abort(msg, call = error_call)
  }

  joiner(!!!pred)

}

## Return the union or intersection of predicate expressions.
##
## `all_exprs()` and `any_exprs()` take predicate expressions and join them
## into a single predicate. They assume vectorised expressions by
## default and join them with `&` or `|`. Note that this will also
## work with scalar predicates, but if you want to be explicit you can
## set `.vectorised` to `FALSE` to join by `&&` or `||`.
##
## @param ... Predicate expressions.
## @param .vectorised If `TRUE`, predicates are joined with `&` or
##   `|`. Otherwise, they are joined with `&&` or `||`.
## @return A [quosure][rlang::quo].
## @export
## @examples
## all_exprs(cyl > 3, am == 1)
## any_exprs(cyl > 3, am == 1)
## any_exprs(cyl > 3, am == 1, .vectorised = FALSE)
all_exprs <- function(..., .vectorised = TRUE) {
  op <- if (.vectorised) quote(`&`) else quote(`&&`)
  quo_reduce(..., .op = op)
}
## @rdname all_exprs
## @export
any_exprs <- function(..., .vectorised = TRUE) {
  op <- if (.vectorised) quote(`|`) else quote(`||`)
  quo_reduce(..., .op = op)
}

## @param .op Can be a function or a quoted name of a function. If a
##   quoted name, the default environment is the [base
##   environment][rlang::base_env] unless you supply a
##   [quosure][rlang::quo].
quo_reduce <- function(..., .op) {
  stopifnot(is_symbol(.op) || is_function(.op))

  dots <- enquos(...)
  if (length(dots) == 1) {
    return(dots[[1]])
  }

  op_quo <- as_quosure(.op, base_env())
  op <- quo_get_expr(op_quo)

  expr <- reduce(dots, function(x, y) expr((!!op)((!!x), (!!y))))
  new_quosure(expr, quo_get_env(op_quo))
}
