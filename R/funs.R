#' Create a list of functions calls.
#'
#' `funs()` provides a flexible way to generate a named list of
#' functions for input to other functions like [summarise_at()].
#'
#' @param ... A list of functions specified by:
#'
#'  - Their name, `"mean"`
#'  - The function itself, `mean`
#'  - A call to the function with `.` as a dummy argument,
#'    `mean(., na.rm = TRUE)`
#'
#'   These arguments are automatically [quoted][rlang::quo]. They
#'   support [unquoting][rlang::quasiquotation] and splicing. See
#'   `vignette("programming")` for an introduction to these concepts.
#' @param .args,args A named list of additional arguments to be added
#'   to all function calls.
#' @export
#' @examples
#' funs(mean, "mean", mean(., na.rm = TRUE))
#'
#' # Override default names
#' funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE))
#'
#' # If you have function names in a vector, use funs_
#' fs <- c("min", "max")
#' funs_(fs)
funs <- function(..., .args = list()) {
  dots <- quos(...)
  default_env <- caller_env()

  funs <- map(dots, function(quo) as_fun(quo, default_env, .args))
  new_funs(funs)
}
new_funs <- function(funs) {
  names(funs) <- names2(funs)

  missing_names <- names(funs) == ""
  default_names <- map_chr(funs[missing_names], function(dot) {
    quo_name(node_car(f_rhs(dot)))
  })
  names(funs)[missing_names] <- default_names

  class(funs) <- "fun_list"
  attr(funs, "have_name") <- any(!missing_names)
  funs
}

as_fun_list <- function(.x, .quo, .env, ...) {
  # Capture quosure before evaluating .x
  force(.quo)

  # If a fun_list, update args
  args <- dots_list(...)
  if (is_fun_list(.x)) {
    if (!is_empty(args)) {
      .x[] <- map(.x, lang_modify, !!! args)
    }
    return(.x)
  }

  # Take functions by expression if they are supplied by name. This
  # way we can evaluate it hybridly.
  if (is_function(.x) && quo_is_symbol(.quo)) {
    .x <- list(.quo)
  } else if (is_character(.x)) {
    .x <- as.list(.x)
  } else if (is_bare_formula(.x, lhs = FALSE)) {
    .x <- list(as_function(.x))
  } else if (!is_list(.x)) {
    .x <- list(.x)
  }

  funs <- map(.x, as_fun, .env = fun_env(.quo, .env), args)
  new_funs(funs)
}

as_fun <- function(.x, .env, .args) {
  quo <- as_quosure(.x, .env)

  # For legacy reasons, we support strings. Those are enclosed in the
  # empty environment and need to be switched to the caller environment.
  f_env(quo) <- fun_env(quo, .env)

  expr <- get_expr(.x)
  if (is_lang(expr) && !is_lang(expr, c("::", ":::"))) {
    expr <- lang_modify(expr, !!! .args)
  } else {
    expr <- lang(expr, quote(.), !!! .args)
  }

  set_expr(quo, expr)
}

fun_env <- function(quo, default_env) {
  env <- f_env(quo)
  if (is_null(env) || identical(env, empty_env())) {
    default_env
  } else {
    env
  }
}

is_fun_list <- function(x, env) {
  inherits(x, "fun_list")
}
#' @export
`[.fun_list` <- function(x, i) {
  structure(NextMethod(),
    class = "fun_list",
    has_names = attr(x, "has_names")
  )
}
#' @export
print.fun_list <- function(x, ..., width = getOption("width")) {
  cat("<fun_calls>\n")
  names <- format(names(x))

  code <- map_chr(x, function(x) deparse_trunc(f_rhs(x), width - 2 - nchar(names[1])))

  cat(paste0("$ ", names, ": ", code, collapse = "\n"))
  cat("\n")
  invisible(x)
}

#' @export
#' @rdname se-deprecated
#' @inheritParams funs
#' @param env The environment in which functions should be evaluated.
funs_ <- function(dots, args = list(), env = base_env()) {
  dots <- compat_lazy_dots(dots, caller_env())
  funs(!!! dots, .args = args)
}
