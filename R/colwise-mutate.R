#' Summarise and mutate multiple columns.
#'
#' @description
#'
#' * `summarise_all()`, `mutate_all()` and `transmute_all()` apply the
#'   functions to all (non-grouping) columns.
#'
#' * `summarise_at()`, `mutate_at()` and `transmute_at()` allow you to
#'   select columns using the same name-based [select_helpers] just
#'   like with [select()].
#'
#' * `summarise_if`(), `mutate_if`() and `transmute_if()` operate on
#'   columns for which a predicate returns `TRUE`.
#'
#' * [summarise_each()] and [mutate_each()] are older variants that
#'   are now deprecated.
#'
#' @inheritParams scoped
#' @param .cols This argument has been renamed to `.vars` to fit
#'   dplyr's terminology and is deprecated.
#' @return A data frame. By default, the newly created columns have the shortest
#'   names needed to distinguish the output. To force inclusion of a name,
#'   even when not needed, name the input (see examples for details).
#' @seealso [vars()], [funs()]
#' @examples
#' by_species <- iris %>% group_by(Species)
#'
#' # One function
#' by_species %>% summarise_all(n_distinct)
#' by_species %>% summarise_all(mean)
#'
#' # Use the _at and _if variants for conditional mapping.
#' by_species %>% summarise_if(is.numeric, mean)
#'
#' # summarise_at() can use select() helpers with the vars() function:
#' by_species %>% summarise_at(vars(Petal.Width), mean)
#' by_species %>% summarise_at(vars(matches("Width")), mean)
#'
#' # You can also specify columns with column names or column positions:
#' by_species %>% summarise_at(c("Sepal.Width", "Petal.Width"), mean)
#' by_species %>% summarise_at(c(1, 3), mean)
#'
#' # You can provide additional arguments. Those are evaluated only once:
#' by_species %>% summarise_all(mean, trim = 1)
#' by_species %>% summarise_at(vars(Petal.Width), mean, trim = 1)
#'
#' # You can provide an expression or multiple functions with the funs() helper.
#' by_species %>% mutate_all(funs(. * 0.4))
#' by_species %>% summarise_all(funs(min, max))
#' # Note that output variable name must now include function name, in order to
#' # keep things distinct.
#'
#' # Function names will be included if .funs has names or whenever multiple
#' # functions are used.
#' by_species %>% mutate_all(funs("in" = . / 2.54))
#' by_species %>% mutate_all(funs(rg = diff(range(.))))
#' by_species %>% summarise_all(funs(med = median))
#' by_species %>% summarise_all(funs(Q3 = quantile), probs = 0.75)
#' by_species %>% summarise_all(c("min", "max"))
#'
#' # Two functions, continued
#' by_species %>% summarise_at(vars(Petal.Width, Sepal.Width), funs(min, max))
#' by_species %>% summarise_at(vars(matches("Width")), funs(min, max))
#'
#'
#' # Unlike mutating verbs, the transmute variants discard original
#' # variables when the computations have new names:
#' mutate_if(as_tibble(iris), is.numeric, funs(mean, sd))
#' transmute_if(as_tibble(iris), is.numeric, funs(mean, sd))
#' @aliases summarise_each_q mutate_each_q
#' @export
summarise_all <- function(.tbl, .funs, ...) {
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ...)
  summarise(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
mutate_all <- function(.tbl, .funs, ...) {
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ...)
  mutate(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
transmute_all <- function(.tbl, .funs, ...) {
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ...)
  transmute(.tbl, !!! funs)
}
manip_all <- function(.tbl, .funs, .quo, .env, ...) {
  syms <- syms(tbl_nongroup_vars(.tbl))
  funs <- as_fun_list(.funs, .quo, .env, ...)
  manip_apply_syms(funs, syms, .tbl)
}

#' @rdname summarise_all
#' @export
summarise_if <- function(.tbl, .predicate, .funs, ...) {
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
  summarise(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
mutate_if <- function(.tbl, .predicate, .funs, ...) {
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
  mutate(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
transmute_if <- function(.tbl, .predicate, .funs, ...) {
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
  transmute(.tbl, !!! funs)
}
manip_if <- function(.tbl, .predicate, .funs, .quo, .env, ...) {
  vars <- tbl_if_syms(.tbl, .predicate, .env)
  funs <- as_fun_list(.funs, .quo, .env, ...)
  manip_apply_syms(funs, vars, .tbl)
}

#' @rdname summarise_all
#' @export
summarise_at <- function(.tbl, .vars, .funs, ..., .cols = NULL) {
  .vars <- check_dot_cols(.vars, .cols)
  funs <- manip_at(.tbl, .vars, .funs, enquo(.funs), caller_env(), ...)
  summarise(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
mutate_at <- function(.tbl, .vars, .funs, ..., .cols = NULL) {
  .vars <- check_dot_cols(.vars, .cols)
  funs <- manip_at(.tbl, .vars, .funs, enquo(.funs), caller_env(), ...)
  mutate(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
transmute_at <- function(.tbl, .vars, .funs, ..., .cols = NULL) {
  .vars <- check_dot_cols(.vars, .cols)
  funs <- manip_at(.tbl, .vars, .funs, enquo(.funs), caller_env(), ...)
  transmute(.tbl, !!! funs)
}
manip_at <- function(.tbl, .vars, .funs, .quo, .env, ...) {
  syms <- tbl_at_syms(.tbl, .vars)
  funs <- as_fun_list(.funs, .quo, .env, ...)
  manip_apply_syms(funs, syms, .tbl)
}
check_dot_cols <- function(vars, cols) {
  if (is_null(cols)) {
    vars
  } else {
    warn("`.cols` has been renamed and is deprecated, please use `.vars`")
    if (missing(vars)) cols else vars
  }
}

#' @rdname summarise_all
#' @export
summarize_all <- summarise_all
#' @rdname summarise_all
#' @export
summarize_at <- summarise_at
#' @rdname summarise_all
#' @export
summarize_if <- summarise_if


manip_apply_syms <- function(funs, syms, tbl) {
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


#' Summarise and mutate multiple columns.
#'
#' Apply one or more functions to one or more columns. Grouping variables
#' are always excluded from modification.
#'
#' `mutate_each()` and `summarise_each()` are deprecated in favour of
#' a more featureful family of functions: [mutate_all()],
#' [mutate_at()], [mutate_if()], [summarise_all()], [summarise_at()]
#' and [summarise_if()].
#' @param tbl a tbl
#' @param funs List of function calls, generated by [funs()], or
#'   a character vector of function names.
#' @param ... Variables to include/exclude in mutate/summarise.
#'   You can use same specifications as in [select()]. If missing,
#'   defaults to all non-grouping variables.
#'
#'   For standard evaluation versions (ending in `_`) these can
#'   be either a list of expressions or a character vector.
#' @export
summarise_each <- function(tbl, funs, ...) {
  summarise_each_(tbl, funs, quos(...))
}
#' @export
#' @rdname se-deprecated
#' @inheritParams summarise_each
summarise_each_ <- function(tbl, funs, vars) {
  .Deprecated("summarise_all")
  if (is_empty(vars)) {
    vars <- tbl_nongroup_vars(tbl)
  } else {
    vars <- compat_lazy_dots(vars, caller_env())
    vars <- select_vars(tbl_nongroup_vars(tbl), !!! vars)
  }
  if (is_character(funs)) {
    funs <- funs_(funs)
  }

  funs <- manip_apply_syms(funs, syms(vars), tbl)
  summarise(tbl, !!! funs)
}

#' @export
#' @rdname summarise_each
mutate_each <- function(tbl, funs, ...) {
  if (is_character(funs)) {
    funs <- funs_(funs)
  }

  mutate_each_(tbl, funs, quos(...))
}
#' @export
#' @rdname se-deprecated
mutate_each_ <- function(tbl, funs, vars) {
  .Deprecated("mutate_all")
  if (is_empty(vars)) {
    vars <- tbl_nongroup_vars(tbl)
  } else {
    vars <- compat_lazy_dots(vars, caller_env())
    vars <- select_vars(tbl_nongroup_vars(tbl), !!! vars)
  }
  funs <- manip_apply_syms(funs, syms(vars), tbl)
  mutate(tbl, !!! funs)
}

#' @export
summarise_each_q <- function(...) {
  .Deprecated("summarise_all")
  summarise_each_(...)
}
#' @export
mutate_each_q <- function(...) {
  .Deprecated("mutate_all")
  mutate_each_(...)
}

#' @rdname summarise_each
#' @export
summarize_each <- summarise_each
#' @rdname se-deprecated
#' @export
summarize_each_ <- summarise_each_
