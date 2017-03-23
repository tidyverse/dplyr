
all_of <- function(..., .vectorised = TRUE) {
  op <- if (.vectorised) quote(`&`) else quote(`&&`)
  quo_reduce(..., .op = op)
}
any_of <- function(..., .vectorised = TRUE) {
  op <- if (.vectorised) quote(`|`) else quote(`||`)
  quo_reduce(..., .op = op)
}

## @param .op Can be a function or a quoted name of a function. If a
##   quoted name, the default environment is the [base
##   environment][rlang::base_env] unless you supply a
##   [quosure][rlang::quosure].
quo_reduce <- function(..., .op) {
  stopifnot(is_symbol(.op) || is_function(.op))

  dots <- dots_quosures(...)
  if (length(dots) == 0) {
    abort("There should be at least one expression")
  } else if (length(dots) == 1) {
    return(dots[[1]])
  }

  op_quo <- as_quosure(.op, base_env())
  op <- f_rhs(op_quo)

  expr <- reduce(dots, function(x, y) expr(UQ(op)((!! x), (!! y))))
  new_quosure(expr, f_env(op_quo))
}
