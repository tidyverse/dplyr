#' Create a list of functions calls.
#'
#' \code{funs} provides a flexible way to generate a named list of functions for
#' input to other functions like \code{summarise_each}.
#'
#' @param dots,... A list of functions specified by:
#'
#'   \itemize{
#'     \item Their name, \code{"mean"}
#'     \item The function itself, \code{mean}
#'     \item A call to the function with \code{.} as a dummy parameter,
#'       \code{mean(., na.rm = TRUE)}
#'   }
#' @export
#' @examples
#' funs(mean, "mean", mean(., na.rm = TRUE))
#'
#' # Overide default names
#' funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE))
#'
#' # If you have function names in a vector, use funs_
#' fs <- c("min", "max")
#' funs_(fs)
funs <- function(...) funs_(lazyeval::lazy_dots(...))

#' @export
#' @rdname funs
funs_ <- function(dots) {
  dots <- lazyeval::as.lazy_dots(dots)
  env <- lazyeval::common_env(dots)

  names(dots) <- names2(dots)

  dots[] <- lapply(dots, function(x) {
    x$expr <- make_call(x$expr)
    x
  })

  missing_names <- names(dots) == ""
  default_names <- vapply(dots[missing_names], function(x) make_name(x$expr),
    character(1))
  names(dots)[missing_names] <- default_names

  class(dots) <- c("fun_list", "lazy_dots")
  dots
}

is.fun_list <- function(x, env) inherits(x, "fun_list")

as.fun_list <- function(x, env) UseMethod("as.fun_list")
#' @export
as.fun_list.fun_list <- function(x, env) x
#' @export
as.fun_list.character <- function(x, env) {
  parsed <- lapply(x, function(x) parse(text = x)[[1]])
  funs_(parsed)
}

#' @export
print.fun_list <- function(x, ..., width = getOption("width")) {
  cat("<fun_calls>\n")
  names <- format(names(x))

  code <- vapply(x, function(x) {
    deparse_trunc(x$expr, width - 2 - nchar(names[1]))
  }, character(1))

  cat(paste0("$ ", names, ": ", code, collapse = "\n"))
  cat("\n")
  invisible(x)
}

make_call <- function(x) {
  if (is.character(x)) {
    substitute(f(.), list(f = as.name(x)))
  } else if (is.name(x)) {
    substitute(f(.), list(f = x))
  } else if (is.call(x)) {
    x
  } else {
    stop("Unknown inputs")
  }
}
make_name <- function(x) {
  if (is.character(x)) {
    x
  } else if (is.name(x)) {
    as.character(x)
  } else if (is.call(x)) {
    as.character(x[[1]])
  } else {
    stop("Unknown input:", class(x)[1])
  }
}
