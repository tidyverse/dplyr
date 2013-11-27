#' A helper function for ordering window function output.
#' 
#' This is a useful function to control the order of window functions in 
#' R that don't have a specific ordering parameter. When translated to SQL
#' it will modify the order clause of the OVER function. 
#' 
#' This function works by changing the \code{call} to instead call
#' \code{\link{with_order}} with the appropriate arguments.
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
  call <- substitute(call)
  stopifnot(is.call(call))
  
  new_call <- as.call(c(quote(with_order), substitute(order_by), as.list(call)))
  eval(new_call, parent.frame())
}

#' Run a function with one order, translating result back to original order
#' 
#' This is used to power the ordering parameters of dplyr's window functions
#' 
#' @param order_by vector to order by
#' @param fun window function
#' @param x,... arguments to \code{f}
#' @keywords internal
#' @export
with_order <- function(order_by, fun, x, ...) {
  ord <- order(order_by)
  undo <- match(seq_along(order_by), ord)
  
  out <- fun(x[ord], ...)
  out[undo]
}
