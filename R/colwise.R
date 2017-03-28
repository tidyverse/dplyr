#' Select columns
#'
#' This helper is intended to provide equivalent semantics to
#' [select()]. Its purpose is to provide `select()` semantics to the
#' colwise summarising and mutating verbs.
#'
#' Note that verbs accepting a `vars()` specification also accept an
#' [integerish][rlang::integerish] vector of positions or a character
#' vector of column names.
#'
#' @param ... Variables to include/exclude in mutate/summarise. You
#'   can use same specifications as in [select()]. If
#'   missing, defaults to all non-grouping variables.
#' @seealso [summarise_all()]
#' @export
vars <- function(...) {
  quos(...)
}

# Requires tbl_vars() method
tbl_at_syms <- function(tbl, vars) {
  tibble_vars <- tbl_vars(tbl, group_vars = FALSE)

  if (is_character(vars)) {
    syms(vars)
  } else if (is_integerish(vars)) {
    syms(tibble_vars[vars])
  } else if (is_quosures(vars)) {
    syms(select_vars(tibble_vars, !!! vars))
  } else {
    abort("`.cols` should be a character/numeric vector or a columns object")
  }
}

# Requires tbl_vars(), `[[`() and length() methods
tbl_if_syms <- function(.tbl, .p, ...) {
  vars <- tbl_vars(.tbl, group_vars = FALSE)

  if (is_logical(.p)) {
    stopifnot(length(.p) == length(vars))
    return(syms(vars[.p]))
  }

  if (inherits(.tbl, "tbl_lazy")) {
    inform("Applying predicate on the first 100 rows")
    .tbl <- collect(.tbl, n = 100)
  }

  n <- length(.tbl)
  selected <- lgl_len(n)
  for (i in seq_len(n)) {
    selected[[i]] <- .p(.tbl[[i]], ...)
  }

  vars <- vars[selected]
  syms(vars)
}

apply_vars <- function(funs, vars, named_vars, tbl) {
  stopifnot(is_fun_list(funs))

  named_calls <- attr(funs, "have_name")
  vars <- select_vars(tbl_vars(tbl, group_vars = FALSE), !!! vars)

  out <- vector("list", length(vars) * length(funs))
  dim(out) <- c(length(vars), length(funs))

  for (i in seq_along(vars)) {
    for (j in seq_along(funs)) {
      var_sym <- sym(vars[[i]])
      out[[i, j]] <- expr_substitute(funs[[j]], quote(.), var_sym)
    }
  }
  dim(out) <- NULL

  if (length(funs) == 1 && !named_calls) {
    names(out) <- names(vars)
  } else if (length(vars) == 1 && !named_vars) {
    names(out) <- names(funs)
  } else {
    grid <- expand.grid(var = names(vars), call = names(funs))
    names(out) <- paste(grid$var, grid$call, sep = "_")
  }

  out
}
