#' Create a list of functions calls.
#'
#' `funs()` provides a flexible way to generate a named list of functions for
#' input to other functions like [summarise_at()].
#'
#' @param dots,... A list of functions specified by:
#'
#'   \itemize{
#'     \item Their name, `"mean"`
#'     \item The function itself, `mean`
#'     \item A call to the function with `.` as a dummy argument,
#'       `mean(., na.rm = TRUE)`
#'   }
#' @param args A named list of additional arguments to be added to all
#'   function calls.
#' @param env The environment in which functions should be evaluated.
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
funs <- function(...) funs_(lazyeval::lazy_dots(...))

#' @export
#' @rdname funs
funs_ <- function(dots, args = list(), env = base_env()) {
  dots <- lazyeval::as.lazy_dots(dots, env)
  env <- lazyeval::common_env(dots)

  names(dots) <- names2(dots)

  dots[] <- map(dots, function(x) {
    x$expr <- make_call(x$expr, args)
    x
  })

  missing_names <- names(dots) == ""
  default_names <- map_chr(dots[missing_names], function(x) make_name(x$expr))
  names(dots)[missing_names] <- default_names

  class(dots) <- c("fun_list", "lazy_dots")
  attr(dots, "has_names") <- any(!missing_names)
  dots
}

is_fun_list <- function(x, env) inherits(x, "fun_list")

as_fun_list <- function(.x, ..., .env = base_env()) {
  UseMethod("as_fun_list")
}
#' @export
as_fun_list.fun_list <- function(.x, ..., .env = base_env()) {
  .x[] <- map(.x, function(fun) {
    fun$expr <- merge_args(fun$expr, list(...))
    fun
  })

  .x
}
#' @export
as_fun_list.character <- function(.x, ..., .env = base_env()) {
  parsed <- map(.x, parse_expr)
  funs_(parsed, list(...), .env)
}
#' @export
as_fun_list.function <- function(.x, ..., .env = base_env()) {
  .env <- new_env(.env)
  .env$`__dplyr_colwise_fun` <- .x

  call <- make_call("__dplyr_colwise_fun", list(...))
  dots <- lazyeval::as.lazy_dots(call, .env)

  funs_(dots)
}

#' @export
`[.fun_list` <- function(x, i) {
  structure(
    NextMethod(),
    class = c("fun_list", "lazy_dots"),
    has_names = attr(x, "has_names")
  )
}

#' @export
print.fun_list <- function(x, ..., width = getOption("width")) {
  cat("<fun_calls>\n")
  names <- format(names(x))

  code <- map_chr(x, function(x) deparse_trunc(x$expr, width - 2 - nchar(names[1])))

  cat(paste0("$ ", names, ": ", code, collapse = "\n"))
  cat("\n")
  invisible(x)
}

make_call <- function(x, args) {
  if (is_string(x)) {
    call <- substitute(f(.), list(f = symbol(x)))
  } else if (is_name(x)) {
    call <- substitute(f(.), list(f = x))
  } else if (is_call(x)) {
    call <- x
  } else {
    abort("Unknown inputs")
  }

  merge_args(call, args)
}
make_name <- function(x) {
  if (is_character(x)) {
    x
  } else if (is_name(x)) {
    as_character(x)
  } else if (is_call(x)) {
    as_character(x[[1]])
  } else {
    abort("Unknown input:", class(x)[1])
  }
}

merge_args <- function(call, args) {
  if (!length(args)) {
    return(call)
  }

  index <- seq(length(call) + 1, length(call) + length(args))
  call[index] <- args
  names(call)[index] <- names2(args)

  call
}
