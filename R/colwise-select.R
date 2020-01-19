#' Select and rename a selection of variables
#'
#' @description
#' These [scoped] variants of [select()] and [rename()] operate on a
#' selection of variables. The semantics of these verbs have subtle
#' but important differences:
#'
#' * Selection drops variables that are not in the selection while
#'   renaming retains them.
#'
#' * The renaming function is optional for selection but not for
#'   renaming.
#'
#' The `_if` and `_at` variants always retain grouping variables for grouped
#' data frames.
#'
#' @inheritParams scoped
#' @param .funs A function `fun`, a purrr style lambda `~ fun(.)` or a list of either form.
#'
#' @section Grouping variables:
#'
#' Existing grouping variables are always kept in the data frame, even
#' if not included in the selection.
#'
#' @examples
#'
#' # Supply a renaming function:
#' select_all(mtcars, toupper)
#' select_all(mtcars, "toupper")
#' select_all(mtcars, list(~toupper(.)))
#'
#' # Selection drops unselected variables:
#' is_whole <- function(x) all(floor(x) == x)
#' select_if(mtcars, is_whole, toupper)
#' select_at(mtcars, vars(-contains("ar"), starts_with("c")), toupper)
#'
#' # But renaming retains them:
#' rename_if(mtcars, is_whole, toupper)
#' rename_at(mtcars, vars(-(1:3)), toupper)
#' rename_all(mtcars, toupper)
#'
#' # The renaming function is optional for selection:
#' select_if(mtcars, is_whole)
#' select_at(mtcars, vars(-everything()))
#' select_all(mtcars)
#' @export
select_all <- function(.tbl, .funs = list(), ...) {
  funs <- as_fun_list(.funs, caller_env(), ...)
  vars <- tbl_vars(.tbl)
  syms <- vars_select_syms(vars, funs, .tbl)
  select(.tbl, !!!syms)
}
#' @rdname select_all
#' @export
rename_all <- function(.tbl, .funs = list(), ...) {
  funs <- as_fun_list(.funs, caller_env(), ...)
  vars <- tbl_vars(.tbl)
  syms <- vars_select_syms(vars, funs, .tbl, strict = TRUE)
  rename(.tbl, !!!syms)
}

#' @rdname select_all
#' @export
select_if <- function(.tbl, .predicate, .funs = list(), ...) {
  funs <- as_fun_list(.funs, caller_env(), ...)
  if (!is_logical(.predicate)) {
    .predicate <- as_fun_list(.predicate, caller_env())
  }
  vars <- tbl_if_vars(.tbl, .predicate, caller_env(), .include_group_vars = TRUE)
  syms <- vars_select_syms(vars, funs, .tbl)
  select(.tbl, !!!syms)
}
#' @rdname select_all
#' @export
rename_if <- function(.tbl, .predicate, .funs = list(), ...) {
  funs <- as_fun_list(.funs, caller_env(), ...)
  if (!is_logical(.predicate)) {
    .predicate <- as_fun_list(.predicate, caller_env())
  }
  vars <- tbl_if_vars(.tbl, .predicate, caller_env(), .include_group_vars = TRUE)
  syms <- vars_select_syms(vars, funs, .tbl, strict = TRUE)
  rename(.tbl, !!!syms)
}

#' @rdname select_all
#' @export
select_at <- function(.tbl, .vars, .funs = list(), ...) {
  vars <- tbl_at_vars(.tbl, .vars, .include_group_vars = TRUE)
  funs <- as_fun_list(.funs, caller_env(), ...)
  syms <- vars_select_syms(vars, funs, .tbl)
  select(.tbl, !!!syms)
}
#' @rdname select_all
#' @export
rename_at <- function(.tbl, .vars, .funs = list(), ...) {
  vars <- tbl_at_vars(.tbl, .vars, .include_group_vars = TRUE)
  funs <- as_fun_list(.funs, caller_env(), ...)
  syms <- vars_select_syms(vars, funs, .tbl, strict = TRUE)
  rename(.tbl, !!!syms)
}

vars_select_syms <- function(vars, funs, tbl, strict = FALSE) {
  if (length(funs) > 1) {
    bad_args(".funs", "must contain one renaming function, not {length(funs)}")
  } else if (length(funs) == 1) {
    fun <- funs[[1]]
    if (is_quosure(fun)) {
      fun <- quo_as_function(fun)
    }
    syms <- if (length(vars)) {
      set_names(syms(vars), fun(as.character(vars)))
    } else {
      set_names(syms(vars))
    }
  } else if (!strict) {
    syms <- syms(vars)
  } else {
    bad_args(".funs", "must specify a renaming function")
  }

  group_vars <- group_vars(tbl)
  group_syms <- syms(group_vars)
  has_group_sym <- group_syms %in% syms
  new_group_syms <- set_names(group_syms[!has_group_sym], group_vars[!has_group_sym])
  c(new_group_syms, syms)
}
