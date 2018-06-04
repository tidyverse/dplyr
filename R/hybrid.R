verify_hybrid <- function(x) {
  abort("Not in hybrid evaluation")
}

verify_not_hybrid <- function(x) {
  x
}

with_hybrid <- function(expr, ...) {
  expr <- enquo(expr)
  stopifnot(any(class(expr) == "formula"))
  expr[[2]] <- prepend_call(expr[[2]], "verify_hybrid")
  data <- data_frame(...)

  # Make verify_hybrid() available to the evaluated expression
  eval_env <- new.env(parent = environment(expr))
  eval_env$verify_hybrid <- verify_hybrid
  environment(expr) <- eval_env

  summarise(data, out = !!expr)["out"][[1]]
}

without_hybrid <- function(expr, ...) {
  expr <- enquo(expr)
  stopifnot(any(class(expr) == "formula"))
  expr[[2]] <- prepend_call(expr[[2]], "verify_not_hybrid")
  data <- data_frame(...)

  # Make verify_not_hybrid() available to the evaluated expression
  eval_env <- new.env(parent = environment(expr))
  eval_env$verify_not_hybrid <- verify_not_hybrid
  environment(expr) <- eval_env

  summarise(data, out = !!expr)["out"][[1]]
}

eval_dots <- function(expr, ...) {
  expr <- enquo(expr)
  data <- data_frame(...)
  eval(expr[[2]], data, enclos = environment(expr))
}

# some(func()) -> name(some(func()))
# list(some(func())) -> list(name(some(func())))
prepend_call <- function(expr, name) {
  if (is.call(expr) && expr[[1]] == quote(list)) {
    stopifnot(length(expr) == 2L)
    call("list", call(name, expr[[2]]))
  } else {
    call(name, expr)
  }
}

hybrid_functions <- function(.data){
  # these are updated internally for each group
  ..group_size <- NA_integer_
  ..group_number <- NA_integer_

  # standard R functions using the variables defined above
  n <- function() ..group_size
  row_number <- function(x) {
    if (missing(x)) seq2(1, ..group_size) else dplyr::row_number(x)
  }
  group_indices <- function(.data, ...) {
    if (missing(.data)) rep(..group_number, ..group_size) else dplyr::group_indices(.data, ...)
  }
  ntile <- function(x = row_number(), n) dplyr::ntile(x=x, n=n)

  # returning this environment from which we can
  # - extract the functions n, ...
  # - update the ..variables
  environment()
}

