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
    syms <- syms(vars)
  } else if (is_integerish(vars)) {
    syms <- syms(tibble_vars[vars])
  } else if (is_quosures(vars)) {
    syms <- select_vars(tibble_vars, !!! vars)
    # Forward `vars` names to `syms`. Account for caveat that `syms`
    # might be smaller than `vars`.
    if (any(have_name(vars))) {
      vars <- syms
    } else {
      names(vars) <- NULL
    }
  } else {
    abort("`.cols` should be a character/numeric vector or a `vars()` object")
  }

  set_names(syms, names(vars))
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

apply_syms <- function(funs, syms, tbl) {
  stopifnot(is_fun_list(funs))

  out <- vector("list", length(syms) * length(funs))
  dim(out) <- c(length(syms), length(funs))
  for (i in seq_along(syms)) {
    for (j in seq_along(funs)) {
      var_sym <- sym(syms[[i]])
      out[[i, j]] <- expr_substitute(funs[[j]], quote(.), var_sym)
    }
  }
  dim(out) <- NULL

  if (length(funs) == 1 && !attr(funs, "have_name")) {
    names(out) <- map_chr(syms, as_string)
  } else if (length(syms) == 1 && !is_named(syms)) {
    names(out) <- names(funs)
  } else {
    syms_names <- map_chr(syms, as_string)
    grid <- expand.grid(var = syms_names, call = names(funs))
    names(out) <- paste(grid$var, grid$call, sep = "_")
  }

  out
}
