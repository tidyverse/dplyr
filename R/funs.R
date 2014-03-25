#' Create a list of functions.
#'
#' \code{funs} provides a flexible to generate a named list of functions.
#'
#' @param ... You can specify functions by:
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
funs <- function(..., env = parent.frame()) {
  args <- dots(...)
  names(args) <- names2(args)

  funs <- lapply(args, make_fun, env = env)

  missing_names <- names(funs) == ""
  default_names <- vapply(args[missing_names], make_name, character(1))
  names(funs)[missing_names] <- default_names

  funs
}

make_fun <- function(x, env) {
  if (is.name(x) || is.character(x)) {
    get(as.character(x), envir = env, mode = "function")
  } else if (is.call(x)) {
    dot_fun(x, env)
  } else {
    stop("Unknown inputs")
  }
}
make_name <- function(x, env) {
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

dot_fun <- function(code, env = parent.frame()) {
  args <- pairlist(. = empty_arg())
  eval(call("function", args, code), env)
}

empty_arg <- function() quote(expr = )
