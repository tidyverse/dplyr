#' Create a list of functions calls.
#'
#' `funs()` provides a flexible way to generate a named list of functions for
#' input to other functions like [summarise_at()].
#'
#' @param ... A list of functions specified by:
#'
#'  - Their name, `"mean"`
#'  - The function itself, `mean`
#'  - A call to the function with `.` as a dummy argument,
#'    `mean(., na.rm = TRUE)`
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
  dots <- map(dots, funs_make_call, args = .args)
  new_funs(dots)
}

new_funs <- function(funs) {
  names(funs) <- names2(funs)
  missing_names <- names(funs) == ""
  default_names <- map_chr(funs[missing_names], function(dot) {
    quo_text(node_car(f_rhs(dot)))
  })
  names(funs)[missing_names] <- default_names

  class(funs) <- "fun_list"
  attr(funs, "have_name") <- any(!missing_names)
  funs
}

#' @export
#' @rdname se-deprecated
#' @inheritParams funs
#' @param env The environment in which functions should be evaluated.
funs_ <- function(dots, args = list(), env = base_env()) {
  dots <- compat_lazy_dots(dots, caller_env())
  funs(!!! dots, .args = args)
}

is_fun_list <- function(x, env) {
  inherits(x, "fun_list")
}

as_fun_list <- function(.x, .quo, ...) {
  # Capture quosure before evaluating .x
  force(.quo)

  if (is_fun_list(.x)) {
    .x[] <- map(.x, lang_modify, .args = list(...))
    return(.x)
  }

  funs <- coerce_type(.x, "funs",
    string = ,
    character = map(.x, funs_make_call, list(...), env = f_env(.quo)),
    primitive = ,
    closure = list(funs_make_call(.quo, list(...)))
  )
  new_funs(funs)
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

funs_make_call <- function(x, args, env = base_env()) {
  f <- as_quosureish(x, env)
  expr <- get_expr(x)

  if (is_symbol(expr) || is_lang(expr, c("::", ":::"))) {
    f <- set_expr(f, new_language(expr, quote(.), .args = args))
  } else if (is_string(expr)) {
    f <- set_expr(f, new_language(sym(expr), quote(.), .args = args))
  } else {
    f <- lang_modify(f, .args = args)
  }

  f
}
