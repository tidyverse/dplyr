#' Select and rename a selection of variables
#'
#' @description
#'
#' These [scoped] variants of [select()] and [rename()] operate on a
#' selection of variables. The semantics of these verbs have simple
#' but important differences:
#'
#' * Selection drops variables that are not in the selection while
#'   renaming retains them.
#'
#' * The renaming function is optional for selection but not for
#'   renaming.
#'
#' @inheritParams scoped
#' @param .funs A single expression quoted with [funs()] or within a
#'   quosure, a string naming a function, or a function.
#' @export
#' @examples
#' # Supply a renaming function:
#' select_all(mtcars, toupper)
#' select_all(mtcars, "toupper")
#' select_all(mtcars, funs(toupper(.)))
#'
#' # Selection drops unselected variables:
#' is_whole <- function(x) all(floor(x) == x)
#' select_if(mtcars, is_whole, toupper)
#'
#' # But renaming retains them:
#' rename_if(mtcars, is_whole, toupper)
#'
#' # The renaming function is optional for selection:
#' select_if(mtcars, is_whole)
select_all <- function(.tbl, .funs = list(), ...) {
  funs <- as_fun_list(.funs, enquo(.funs), caller_env(), ...)
  vars <- tbl_nongroup_vars(.tbl)
  syms <- vars_select_syms(vars, funs, .tbl)
  select(.tbl, !!! syms)
}
#' @rdname select_all
#' @export
rename_all <- function(.tbl, .funs = list(), ...) {
  funs <- as_fun_list(.funs, enquo(.funs), caller_env(), ...)
  vars <- tbl_nongroup_vars(.tbl)
  syms <- vars_select_syms(vars, funs, .tbl, strict = TRUE)
  rename(.tbl, !!! syms)
}

#' @rdname select_all
#' @export
select_if <- function(.tbl, .predicate, .funs = list(), ...) {
  funs <- as_fun_list(.funs, enquo(.funs), caller_env(), ...)
  vars <- tbl_if_vars(.tbl, .predicate, caller_env())
  syms <- vars_select_syms(vars, funs, .tbl)
  select(.tbl, !!! syms)
}
#' @rdname select_all
#' @export
rename_if <- function(.tbl, .predicate, .funs = list(), ...) {
  funs <- as_fun_list(.funs, enquo(.funs), caller_env(), ...)
  vars <- tbl_if_vars(.tbl, .predicate, caller_env())
  syms <- vars_select_syms(vars, funs, .tbl, strict = TRUE)
  rename(.tbl, !!! syms)
}

#' @rdname select_all
#' @export
select_at <- function(.tbl, .vars, .funs = list(), ...) {
  vars <- tbl_at_vars(.tbl, .vars)
  funs <- as_fun_list(.funs, enquo(.funs), caller_env(), ...)
  syms <- vars_select_syms(vars, funs, .tbl)
  select(.tbl, !!! syms)
}
#' @rdname select_all
#' @export
rename_at <- function(.tbl, .vars, .funs = list(), ...) {
  vars <- tbl_at_vars(.tbl, .vars)
  funs <- as_fun_list(.funs, enquo(.funs), caller_env(), ...)
  syms <- vars_select_syms(vars, funs, .tbl, strict = TRUE)
  rename(.tbl, !!! syms)
}

vars_select_syms <- function(vars, funs, tbl, strict = FALSE) {
  if (length(funs) > 1) {
    bad_args(".funs", "must contain one renaming function, not {length(funs)}")
  } else if (length(funs) == 1) {
    fun <- as_function(funs[[1]])
    syms <- set_names(syms(vars), fun(vars))
  } else if (!strict) {
    syms <- syms(vars)
  } else {
    bad_args(".funs", "must specify a renaming function")
  }

  group_syms <- base::setdiff(syms(group_vars(tbl)), syms)
  c(group_syms, syms)
}
