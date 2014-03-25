#' Create a list of functions calls.
#'
#' \code{funs} provides a flexible to generate a named list of functions for
#' input to other functions like \code{colwise}.
#'
#' @param calls,... A list of functions specified by:
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
#' # If you have a function names in a vector, use funs_q
#' fs <- c("min", "max")
#' funs_q(fs)
funs <- function(..., env = parent.frame()) funs_q(dots(...), env)

#' @export
#' @rdname funs
funs_q <- function(calls, env = parent.frame()) {
  names(calls) <- names2(calls)

  calls[] <- lapply(calls, make_call)

  missing_names <- names(calls) == ""
  default_names <- vapply(calls[missing_names], make_name, character(1))
  names(calls)[missing_names] <- default_names

  class(calls) <- "fun_list"
  attr(calls, "env") <- env
  calls
}

is.fun_calls <- function(x, env) inherits(x, "fun_list")

as.fun_list <- function(x, env) UseMethod("as.fun_list")
#' @export
as.fun_list.fun_list <- function(x, env) x
#' @export
as.fun_list.character <- function(x, env) {
  parsed <- lapply(x, function(x) parse(text = x)[[1]])
  funs_q(parsed, env)
}

#' @export
print.fun_list <- function(x, ..., width = getOption("width")) {
  cat("<fun_calls>\n")
  names <- format(names(x))

  code <- vapply(x, deparse_trunc, width = width - 2 - nchar(names[1]),
    character(1))

  cat(paste0("$ ", names, ": ", code, collapse = "\n"))
  cat("\n")
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
