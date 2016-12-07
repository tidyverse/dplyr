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
  expr[[2]] <- call("verify_hybrid", expr[[2]])
  data <- data_frame(...)
  summarise_(data, out = expr)["out"][[1]]
}

without_hybrid <- function(expr, ...) {
  .dots <- lazyeval::lazy_dots(out = expr)[[1]]
  without_hybrid_(lazyeval::f_new(.dots$expr, env = .dots$env), ...)
}

without_hybrid_ <- function(expr, ...) {
  stopifnot(any(class(expr) == "formula"))
  expr[[2]] <- call("verify_not_hybrid", expr[[2]])
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
