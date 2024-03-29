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
    if (is_symbol(expr)) {
      bullets <- c(
        glue("`call` must be a function call, not a symbol."),
        i = glue("Did you mean `arrange({as_label(enquo(order_by))}, {expr})`?")
      )
      abort(bullets)
    } else {
      type <- obj_type_friendly(expr)
      msg <- glue("`call` must be a function call, not { type }.")
      abort(msg)
    }
  }

  fn <- set_expr(quo, expr[[1]])
  args <- map(expr[-1], new_quosure, quo_get_env(quo))

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
  vec_check_size(order_by, size = vec_size(x))

  o <- vec_order_radix(order_by)
  x <- vec_slice(x, o)

  out <- fun(x, ...)

  o <- vec_order_radix(o)
  vec_slice(out, o)
}
