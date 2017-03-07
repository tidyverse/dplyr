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
  dots <- tidy_quotes(...)
  dots <- map(dots, funs_make_call, args = .args)

  names(dots) <- names2(dots)
  missing_names <- names(dots) == ""
  default_names <- map_chr(dots[missing_names], as_name)
  names(dots)[missing_names] <- default_names

  class(dots) <- "fun_list"
  attr(dots, "have_names") <- any(!missing_names)
  dots
}

#' @export
#' @rdname se-deprecated
#' @inheritParams funs
#' @param env The environment in which functions should be evaluated.
funs_ <- function(dots, args = list(), env = base_env()) {
  dots <- compat_lazy_dots(dots, caller_env(), .named = TRUE)
  funs(!!! dots, .args = args)
}

is_fun_list <- function(x, env) {
  inherits(x, "fun_list")
}

as_fun_list <- function(.x, ..., .env = base_env()) {
  UseMethod("as_fun_list")
}
#' @export
as_fun_list.fun_list <- function(.x, ..., .env = base_env()) {
  .x[] <- map(.x, lang_modify, .args = list(...))
  .x
}
#' @export
as_fun_list.character <- function(.x, ..., .env = base_env()) {
  funs <- map(.x, funs_make_call, list(...), env = .env)
  funs(!!! funs, .args = list(...))
}
#' @export
as_fun_list.function <- function(.x, ..., .env = base_env()) {
  .env <- child_env(.env)
  .env$`__dplyr_colwise_fun` <- .x

  call <- funs_make_call("__dplyr_colwise_fun", list(...), env = .env)
  funs(!! call)
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

  expr <- switch_type(expr, "funs",
    quote = ,
    language = expr,
    symbol = substitute(f(.), list(f = expr)),
    string = substitute(f(.), list(f = symbol(expr)))
  )

  expr <- lang_modify(expr, .args = args)
  set_expr(f, expr)
}
