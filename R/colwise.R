# colwise(df, "as.POSIXct")
# colwise(df, "as.POSIXct", x, y, z)
# colwise(df, "as.POSIXct", contains('date'))
# colwise(df, c("min", "mean", "max"), x)
# colwise(df, c("min", "mean", "max"), x, y, z)
# colwise(df, c("min", "mean", "max"), contains("date"))
# colwise(df, c("min", "mean", "max"))g
# would be nice to supply additional ...

# This function is the workhorse. Powers all the others.

# Ways to specify functions:
#
#   character vector of names
#   named list of functions
#   special construction that allows you to use .

#' @examples
#' colwise(iris, "mean", dots(Species))
#' colwise(iris, c("min", "max"), dots(matches("Sepal")), na.rm = TRUE)
colwise <- function(tbl, funs, vars, ...) {
  vars <- lapply(select_vars(tbl_vars(tbl), vars, env = parent.frame()), as.name)
  funs <- lapply(funs, as.name)


  calls <- vector("list", length(vars) * length(funs))
  dim(calls) <- c(length(vars), length(funs))

  for (i in seq_along(vars)) {
    for (j in seq_along(funs)) {
      call <- substitute(`__f`(`__x`, ...))
      call[[1]] <- funs[[j]]
      call[[2]] <- vars[[i]]
      calls[[i, j]] <- call
    }
  }

  dim(calls) <- NULL
  calls
}

# colwise(iris, "mean", dots(x))

dotted <- function(..., env = parent.frame()) {
  lapply(dots(...), dot_fun, env)
}

dot_fun <- function(code, env = parent.frame()) {
  args <- list(. = empty_arg())
  if (is.name(code)) {
    code <- substitute(f(.), list(f = code))
  }
  make_function(args, code, env)
}

make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  eval(call("function", args, body), env)
}

empty_arg <- function() quote(expr = )

# funs(mean, "mean", mean(., na.rm = TRUE))
#
# Character vector
# Name of another function
# Otherwise it's a dotted call
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
