## Return the union or intersection of predicate expressions.
##
## `all_exprs()` and `any_exprs()` take predicate expressions and join them
## into a single predicate. They assume vectorised expressions by
## default and join them with `&` or `|`. Note that this will also
## work with scalar predicates, but if you want to be explicit you can
## set `.vectorised` to `FALSE` to join by `&&` or `||`.
##
## @param ... Predicate expressions.
## @param .vectorised If `TRUE`, predicates are joined with `&` or
##   `|`. Otherwise, they are joined with `&&` or `||`.
## @return A [quosure][rlang::quo].
## @export
## @examples
## all_exprs(cyl > 3, am == 1)
## any_exprs(cyl > 3, am == 1)
## any_exprs(cyl > 3, am == 1, .vectorised = FALSE)
all_exprs <- function(..., .vectorised = TRUE) {
  op <- if (.vectorised) quote(`&`) else quote(`&&`)
  quo_reduce(..., .op = op)
}
## @rdname all_exprs
## @export
any_exprs <- function(..., .vectorised = TRUE) {
  op <- if (.vectorised) quote(`|`) else quote(`||`)
  quo_reduce(..., .op = op)
}

## @param .op Can be a function or a quoted name of a function. If a
##   quoted name, the default environment is the [base
##   environment][rlang::base_env] unless you supply a
##   [quosure][rlang::quo].
quo_reduce <- function(..., .op) {
  abort_if_not(is_symbol(.op) || is_function(.op))

  dots <- enquos(...)
  if (length(dots) == 0) {
    abort("At least one expression must be given")
  } else if (length(dots) == 1) {
    return(dots[[1]])
  }

  op_quo <- as_quosure(.op, base_env())
  op <- quo_get_expr(op_quo)

  expr <- reduce(dots, function(x, y) expr((!!op)((!!x), (!!y))))
  new_quosure(expr, quo_get_env(op_quo))
}
