#' Create a list of functions calls.
#'
#' \Sexpr[results=rd, stage=render]{dplyr:::lifecycle("soft-deprecated")}
#'
#' @description
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
#'  These arguments are automatically [quoted][rlang::quo]. They
#'  support [unquoting][rlang::quasiquotation] and splicing. See
#'  `vignette("programming")` for an introduction to these concepts.
#'
#'  The following notations are **not** supported, see examples:
#'
#'  - An anonymous function, `function(x) mean(x, na.rm = TRUE)`
#'  - An anonymous function in \pkg{purrr} notation, `~mean(., na.rm = TRUE)`
#'
#' @param .args,args A named list of additional arguments to be added to all
#'   function calls. As `funs()` is being deprecated, use other methods to
#'   supply arguments: `...` argument in [scoped verbs][summarise_at()] or make
#'   own functions with [purrr::partial()].
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
#'
#' # Not supported
#' \dontrun{
#' funs(function(x) mean(x, na.rm = TRUE))
#' funs(~mean(x, na.rm = TRUE))}
funs <- function(..., .args = list()) {
  lifecycle::deprecate_warn("1.0.0", "funs()", details = paste_line(
    "Please use a list of either functions or lambdas: ",
    "",
    "  # Simple named list: ",
    "  list(mean = mean, median = median)",
    "",
    "  # Auto named with `tibble::lst()`: ",
    "  tibble::lst(mean, median)",
    "",
    "  # Using lambdas",
    "  list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))"
  ))

  dots <- enquos(...)
  default_env <- caller_env()

  funs <- map(dots, function(quo) as_fun(quo, default_env, .args))
  new_funs(funs)
}
new_funs <- function(funs) {
  attr(funs, "have_name") <- any(names2(funs) != "")

  # Workaround until rlang:::label() is exported
  temp <- map(funs, function(fn) node_car(quo_get_expr(fn)))
  temp <- exprs_auto_name(temp)
  names(funs) <- names(temp)

  class(funs) <- "fun_list"
  funs
}

as_fun_list <- function(.funs, .env, ...) {
  args <- list2(...)

  if (is_fun_list(.funs)) {
    if (!is_empty(args)) {
      .funs[] <- map(.funs, call_modify, !!!args)
    }
    return(.funs)
  }

  if (is_list(.funs) && length(.funs) > 1) {
    .funs <- auto_name_formulas(.funs)
  }

  if (!is_character(.funs) && !is_list(.funs)) {
    .funs <- list(.funs)
  }

  if(is_character(.funs) && is_null(names(.funs)) && length(.funs) != 1L) {
    names(.funs) <- .funs
  }

  funs <- map(.funs, function(.x){
    if (is_formula(.x)) {
      if (is_quosure(.x)) {
        signal_soft_deprecated(paste_line(
          "Using quosures is deprecated",
          "Please use a one-sided formula, a function, or a function name"
        ), env = .env)
        .x <- new_formula(NULL, quo_squash(.x), quo_get_env(.x))
      }
      .x <- as_inlined_function(.x, env = .env)
    } else {
      if (is_character(.x)) {
        .x <- get(.x, .env, mode = "function")
      } else if (!is_function(.x)) {
        abort("expecting a one sided formula, a function, or a function name.")
      }
      if (length(args)) {
        .x <- new_quosure(
          call2(.x, quote(.), !!!args),
          env = .env
        )
      }
    }
    .x
  })
  attr(funs, "have_name") <- any(names2(funs) != "")
  funs
}

auto_name_formulas <- function(funs) {
  where <- !have_name(funs) & map_lgl(funs, function(x) is_bare_formula(x) && is_call(f_rhs(x)))
  names(funs)[where] <- map_chr(funs[where], function(x) as_label(f_rhs(x)[[1]]))
  funs
}

as_fun <- function(.x, .env, .args) {
  quo <- as_quosure(.x, .env)

  # For legacy reasons, we support strings. Those are enclosed in the
  # empty environment and need to be switched to the caller environment.
  quo <- quo_set_env(quo, fun_env(quo, .env))

  expr <- quo_get_expr(quo)

  if (is_call(expr, c("function", "~"))) {
    top_level <- as_string(expr[[1]])
    bad_args(quo_text(expr), "must be a function name (quoted or unquoted) or an unquoted call, not `{top_level}`")
  }

  if (is_call(expr) && !is_call(expr, c("::", ":::"))) {
    expr <- call_modify(expr, !!!.args)
  } else {
    expr <- call2(expr, quote(.), !!!.args)
  }

  set_expr(quo, expr)
}

quo_as_function <- function(quo) {
  new_function(exprs(. = ), quo_get_expr(quo), quo_get_env(quo))
}

fun_env <- function(quo, default_env) {
  env <- quo_get_env(quo)
  if (is_null(env) || identical(env, empty_env())) {
    default_env
  } else {
    env
  }
}

is_fun_list <- function(x) {
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

  code <- map_chr(x, function(x) deparse_trunc(quo_get_expr(x), width - 2 - nchar(names[1])))

  cat(paste0("$ ", names, ": ", code, collapse = "\n"))
  cat("\n")
  invisible(x)
}

#' @export
#' @rdname se-deprecated
#' @inheritParams funs
#' @param env The environment in which functions should be evaluated.
funs_ <- function(dots, args = list(), env = base_env()) {
  signal_soft_deprecated(paste_line(
    "funs_() is deprecated. ",
    "Please use list() instead"
  ))

  dots <- compat_lazy_dots(dots, caller_env())
  funs(!!!dots, .args = args)
}

#' Do values in a numeric vector fall in specified range?
#'
#' This is a shortcut for `x >= left & x <= right`, implemented
#' efficiently in C++ for local values, and translated to the
#' appropriate SQL for remote tables.
#'
#' @param x A numeric vector of values
#' @param left,right Boundary values
#' @export
#' @examples
#' between(1:12, 7, 9)
#'
#' x <- rnorm(1e2)
#' x[between(x, -1, 1)]
between <- function(x, left, right) {
  if (!is.double(x)) {
    x <- as.numeric(x)
  }
  .Call(`dplyr_between`, x, as.numeric(left), as.numeric(right))
}

#' Cumulativate versions of any, all, and mean
#'
#' dplyr provides `cumall()`, `cumany()`, and `cummean()` to complete R's set
#' of cumulative functions.
#'
#' @section Cumulative logical functions:
#'
#' These are particularly useful in conjunction with `filter()`:
#'
#' * `cumall(x)`: all cases until the first `FALSE`.
#' * `cumall(!x)`: all cases until the first `TRUE`.
#' * `cumany(x)`: all cases after the first `TRUE`.
#' * `cumany(!x)`: all cases after the first `FALSE`.
#'
#' @param x For `cumall()` and `cumany()`, a logical vector; for
#'   `cummean()` an integer or numeric vector.
#' @return A vector the same length as `x`.
#' @examples
#' # `cummean()` returns a numeric/integer vector of the same length
#' # as the input vector.
#' x <- c(1, 3, 5, 2, 2)
#' cummean(x)
#' cumsum(x) / seq_along(x)
#'
#' # `cumall()` and `cumany()` return logicals
#' cumall(x < 5)
#' cumany(x == 3)
#'
#' # `cumall()` vs. `cumany()`
#' df <- data.frame(
#'   date = as.Date("2020-01-01") + 0:6,
#'   balance = c(100, 50, 25, -25, -50, 30, 120)
#' )
#' # all rows after first overdraft
#' df %>% filter(cumany(balance < 0))
#' # all rows until first overdraft
#' df %>% filter(cumall(!(balance < 0)))
#'
#' @export
cumall <- function(x) {
  .Call(`dplyr_cumall`, as.logical(x))
}

#' @rdname cumall
#' @export
cumany <- function(x) {
  .Call(`dplyr_cumany`, as.logical(x))
}

#' @rdname cumall
#' @export
cummean <- function(x) {
  .Call(`dplyr_cummean`, as.numeric(x))
}
