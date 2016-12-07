verify_hybrid <- function(x) {
  stop("Not in hybrid evaluation", call. = FALSE)
}

verify_not_hybrid <- function(x) {
  x
}

with_hybrid <- function(expr, ...) {
  with_hybrid_(substitute(expr), ...)
}

with_hybrid_ <- function(expr, ...) {
  expr <- eval(call("~", call("verify_hybrid", expr)))
  data <- data_frame(...)
  summarise_(data, out = expr)["out"][[1]]
}

without_hybrid <- function(expr, ...) {
  without_hybrid_(substitute(expr), ...)
}

without_hybrid_ <- function(expr, ...) {
  expr <- eval(call("~", call("verify_not_hybrid", expr)))
  data <- data_frame(...)
  summarise_(data, out = expr)["out"][[1]]
}

eval_dots <- function(expr, ...) {
  eval_dots_(substitute(expr), ...)
}

eval_dots_ <- function(expr, ...) {
  data <- data_frame(...)
  eval(expr, data)
}
