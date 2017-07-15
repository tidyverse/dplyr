#' Summarise and mutate multiple columns.
#'
#' @description
#' These verbs are [scoped] variants of [summarise()], [mutate()] and
#' [transmute()]. They apply operations on a selection of variables.
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
#' @inheritParams scoped
#' @param .cols This argument has been renamed to `.vars` to fit
#'   dplyr's terminology and is deprecated.
#' @return A data frame. By default, the newly created columns have the shortest
#'   names needed to uniquely identify the output. To force inclusion of a name,
#'   even when not needed, name the input (see examples for details).
#' @seealso [vars()], [funs()]
#' @export
#' @examples
#' # The scoped variants of summarise() and mutate() make it easy to
#' # apply the same transformation to multiple variables:
#'
#' iris %>%
#'   group_by(Species) %>%
#'   summarise_all(mean)
#'
#' # There are three variants.
#' # * _all affects every variable
#' # * _at affects variables selected with a character vector or vars()
#' # * _if affects variables selected with a predicate function:
#'
#' starwars %>% summarise_at(vars(height:mass), mean, na.rm = TRUE)
#' starwars %>% summarise_at(c("height", "mass"), mean, na.rm = TRUE)
#' starwars %>% summarise_if(is.numeric, mean, na.rm = TRUE)
#'
#' # mutate_if is particularly useful for transforming variables from
#' # one type to another
#' iris %>% as_tibble() %>% mutate_if(is.factor, as.character)
#' iris %>% as_tibble() %>% mutate_if(is.double, as.integer)
#'
#' # ---------------------------------------------------------------------------
#' # If you want apply multiple transformations, use funs()
#' by_species <- iris %>% group_by(Species)
#'
#' by_species %>% summarise_all(funs(min, max))
#' # Note that output variable name now includes the function name, in order to
#' # keep things distinct.
#'
#' # You can express more complex inline transformations using .
#' by_species %>% mutate_all(funs(. / 2.54))
#'
#' # Function names will be included if .funs has names or multiple inputs
#' by_species %>% mutate_all(funs(cm = . / 2.54))
#' by_species %>% summarise_all(funs(med = median))
#' by_species %>% summarise_all(funs(Q3 = quantile), probs = 0.75)
#' by_species %>% summarise_all(c("min", "max"))
summarise_all <- function(.tbl, .funs, ...) {
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ...)
  summarise(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
summarise_if <- function(.tbl, .predicate, .funs, ...) {
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
  summarise(.tbl, !!! funs)
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
summarize_all <- summarise_all
#' @rdname summarise_all
#' @export
summarize_if <- summarise_if
#' @rdname summarise_all
#' @export
summarize_at <- summarise_at

#' @rdname summarise_all
#' @export
mutate_all <- function(.tbl, .funs, ...) {
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ...)
  mutate(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
mutate_if <- function(.tbl, .predicate, .funs, ...) {
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
  mutate(.tbl, !!! funs)
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
transmute_all <- function(.tbl, .funs, ...) {
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ...)
  transmute(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
transmute_if <- function(.tbl, .predicate, .funs, ...) {
  funs <- manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
  transmute(.tbl, !!! funs)
}
#' @rdname summarise_all
#' @export
transmute_at <- function(.tbl, .vars, .funs, ..., .cols = NULL) {
  .vars <- check_dot_cols(.vars, .cols)
  funs <- manip_at(.tbl, .vars, .funs, enquo(.funs), caller_env(), ...)
  transmute(.tbl, !!! funs)
}

# Helpers -----------------------------------------------------------------

manip_all <- function(.tbl, .funs, .quo, .env, ...) {
  syms <- syms(tbl_nongroup_vars(.tbl))
  funs <- as_fun_list(.funs, .quo, .env, ...)
  manip_apply_syms(funs, syms, .tbl)
}
manip_if <- function(.tbl, .predicate, .funs, .quo, .env, ...) {
  vars <- tbl_if_syms(.tbl, .predicate, .env)
  funs <- as_fun_list(.funs, .quo, .env, ...)
  manip_apply_syms(funs, vars, .tbl)
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
    inform("`.cols` has been renamed and is deprecated, please use `.vars`")
    if (missing(vars)) cols else vars
  }
}

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

  # Use symbols as default names
  unnamed <- !have_name(syms)
  names(syms)[unnamed] <- map_chr(syms[unnamed], as_string)

  if (length(funs) == 1 && !attr(funs, "have_name")) {
    names(out) <- names(syms)
  } else if (length(syms) == 1 && all(unnamed)) {
    names(out) <- names(funs)
  } else {
    syms_names <- map_chr(syms, as_string)
    grid <- expand.grid(var = syms_names, call = names(funs))
    names(out) <- paste(grid$var, grid$call, sep = "_")
  }

  out
}

# Deprecated --------------------------------------------------------------

#' Summarise and mutate multiple columns.
#'
#' @description
#'
#' `mutate_each()` and `summarise_each()` are deprecated in favour of
#' a more featureful family of functions: [mutate_all()],
#' [mutate_at()], [mutate_if()], [summarise_all()], [summarise_at()]
#' and [summarise_if()].
#'
#' The `_each()` functions have two replacements depending on what
#' variables you want to apply `funs` to. To apply a function to all
#' variables, use [mutate_all()] or [summarise_all()]. To apply a
#' function to a selection of variables, use [mutate_at()] or
#' [summarise_at()].
#'
#' See the relevant section of `vignette("compatibility")` for more
#' information.
#'
#' @keywords internal
#' @export
summarise_each <- function(tbl, funs, ...) {
  summarise_each_(tbl, funs, quos(...))
}
#' @export
#' @rdname summarise_each
summarise_each_ <- function(tbl, funs, vars) {
  msg <- glue(
    "`summarise_each()` is deprecated.
     Use `summarise_all()`, `summarise_at()` or `summarise_if()` instead."
  )
  if (is_empty(vars)) {
    inform(glue(msg, "\nTo map `funs` over all variables, use `summarise_all()`"))
    vars <- tbl_nongroup_vars(tbl)
  } else {
    inform(glue(msg, "\nTo map `funs` over a selection of variables, use `summarise_at()`"))
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
#' @rdname summarise_each
mutate_each_ <- function(tbl, funs, vars) {
  msg <- glue(
    "`mutate_each()` is deprecated.
     Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead."
  )
  if (is_empty(vars)) {
    inform(glue(msg, "\nTo map `funs` over all variables, use `mutate_all()`"))
    vars <- tbl_nongroup_vars(tbl)
  } else {
    inform(glue(msg, "\nTo map `funs` over a selection of variables, use `mutate_at()`"))
    vars <- compat_lazy_dots(vars, caller_env())
    vars <- select_vars(tbl_nongroup_vars(tbl), !!! vars)
  }
  funs <- manip_apply_syms(funs, syms(vars), tbl)
  mutate(tbl, !!! funs)
}

#' @rdname summarise_each
#' @export
summarize_each <- summarise_each
#' @rdname summarise_each
#' @export
summarize_each_ <- summarise_each_
