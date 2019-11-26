#' A helper function for ordering window function output
#'
#' This function makes it possible to control the ordering of window functions
#' in R that don't have a specific ordering parameter. When translated to SQL
#' it will modify the order clause of the OVER function.
#'
#' This function works by changing the `call` to instead call
#' [with_order()] with the appropriate arguments.
#'
#' @param order_by a vector to order_by
#' @param call a function call to a window function, where the first argument
#'   is the vector being operated on
#' @export
#' @examples
#' order_by(10:1, cumsum(1:10))
#' x <- 10:1
#' y <- 1:10
#' order_by(x, cumsum(y))
#'
#' df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
#' scrambled <- df[sample(nrow(df)), ]
#'
#' wrong <- mutate(scrambled, running = cumsum(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, running = order_by(year, cumsum(value)))
#' arrange(right, year)
order_by <- function(order_by, call) {
  quo <- enquo(call)
  expr <- quo_get_expr(quo)
  if (!is_call(expr)) {
    type <- friendly_type_of(expr)
    bad_args("call", "must be a function call, not { type }")
  }

  fn <- set_expr(quo, node_car(expr))
  args <- node_cdr(expr)
  args <- map(args, new_quosure, quo_get_env(quo))

  expr <- expr(with_order(!!order_by, !!fn, !!!args))
  eval_tidy(expr)
}

#' Run a function with one order, translating result back to original order
#'
#' This is used to power the ordering parameters of dplyr's window functions
#'
#' @param order_by vector to order by
#' @param fun window function
#' @param x,... arguments to `f`
#' @keywords internal
#' @export
with_order <- function(order_by, fun, x, ...) {
  ord <- vec_order(order_by)
  undo <- vec_match(seq_along(order_by), ord)

  out <- fun(vec_slice(x, ord), ...)
  vec_slice(out, undo)
}
