
all_of <- function(..., .vectorised = FALSE) {
  op <- if (.vectorised) quote(`&`) else quote(`&&`)
  quo_reduce(..., .op = op)
}
any_of <- function(..., .vectorised = FALSE) {
  op <- if (.vectorised) quote(`|`) else quote(`||`)
  quo_reduce(..., .op = op)
}

## @rdname .op Can be a function or a quoted name of a function. If a
##   quoted name, the default environment is the [base
##   environment][rlang::base_env] unless you supply a
##   [quosure][rlang::quosure].
quo_reduce <- function(..., .op) {
  stopifnot(is_symbol(.op) || is_function(.op))

  dots <- dots_quosures(...)
  if (length(dots) == 1) {
    return(dots[[1]])
  } else if (!length(dots)) {
    abort("There should be at least one predicate expression")
  }

  op_quo <- as_quosure(.op, base_env())
  op <- f_rhs(op_quo)

  expr <- reduce(dots, function(x, y) expr(UQ(op)((!! x), (!! y))))
  new_quosure(expr, f_env(op_quo))
}
