verify_hybrid <- function(x) {
  stop("Not in hybrid evaluation", call. = FALSE)
}

verify_not_hybrid <- function(x) {
  x
}

with_hybrid <- function(expr, ...) {
  with_hybrid_(lazyeval::f_capture(expr), ...)
}

with_hybrid_ <- function(expr, ...) {
  stopifnot(any(class(expr) == "formula"))
  expr[[2]] <- prepend_call(expr[[2]], "verify_hybrid")
  data <- data_frame(...)
  summarise_(data, out = expr)["out"][[1]]
}

without_hybrid <- function(expr, ...) {
  without_hybrid_(lazyeval::f_capture(expr), ...)
}

without_hybrid_ <- function(expr, ...) {
  stopifnot(any(class(expr) == "formula"))
  expr[[2]] <- prepend_call(expr[[2]], "verify_not_hybrid")
  data <- data_frame(...)
  summarise_(data, out = expr)["out"][[1]]
}

eval_dots <- function(expr, ...) {
  eval_dots_(lazyeval::f_capture(expr), ...)
}

eval_dots_ <- function(expr, ...) {
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
