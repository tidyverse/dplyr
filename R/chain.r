#' Chain together multiple operations.
#' 
#' The downside of the functional nature of dplyr is that when you combine
#' multiple data manipulation operations, you have to read from the inside
#' out and the arguments may be very distant to the function call. These 
#' functions providing an alternative way of calling dplyr (and other data
#' manipulation) functions that you read can from left to right.
#' 
#' The functions work via simple substitution so that \code{chain(x, f(y))} or
#' \code{x \%.\% f(y)} is translated into \code{f(x, y)}.
#' 
#' @param x,y A dataset and function to apply to it
#' @param ...,calls A sequence of data transformations, starting with a dataset.
#'   The first argument of each call should be omitted - the value of the
#'   previous step will be substituted in automatically.  Use \code{chain} and 
#'   \code{...} when working interactive; use \code{chain_q} and \code{calls}
#'   when calling from another function.
#' @param env Environment in which to evaluation expressions. In ordinary
#'   operation you should not need to set this parameter.
#' @export
#' @examples
#' # If you're performing many operations you can either do step by step
#' a1 <- group_by(hflights, Year, Month, DayofMonth)
#' a2 <- select(a1, Year:DayofMonth, ArrDelay, DepDelay)
#' a3 <- summarise(a2, 
#'   arr = mean(ArrDelay, na.rm = TRUE), 
#'   dep = mean(DepDelay, na.rm = TRUE))
#' a4 <- filter(a3, arr > 30 | dep > 30)
#' 
#' # If you don't want to save the intermediate results, you need to 
#' # wrap the functions:
#' filter(
#'   summarise(
#'     select(
#'       group_by(hflights, Year, Month, DayofMonth), 
#'       Year:DayofMonth, ArrDelay, DepDelay
#'     ), 
#'     arr = mean(ArrDelay, na.rm = TRUE),  
#'     dep = mean(DepDelay, na.rm = TRUE)
#'   ), 
#'   arr > 30 | dep > 30
#' )
#' 
#' # This is difficult to read because the order of the operations is from
#' # inside to out, and the arguments are a long way away from the function.
#' # Alternatively you can use chain or %.% to sequence the operations
#' # linearly:
#' 
#' hflights %.%
#'   group_by(Year, Month, DayofMonth) %.%
#'   select(Year:DayofMonth, ArrDelay, DepDelay) %.%
#'   summarise(
#'     arr = mean(ArrDelay, na.rm = TRUE), 
#'     dep = mean(DepDelay, na.rm = TRUE)
#'   ) %.%
#'   filter(arr > 30 | dep > 30)
#'
#' chain(
#'   hflights,
#'   group_by(Year, Month, DayofMonth),
#'   select(Year:DayofMonth, ArrDelay, DepDelay),
#'   summarise(
#'     arr = mean(ArrDelay, na.rm = TRUE), 
#'     dep = mean(DepDelay, na.rm = TRUE)
#'   ),
#'   filter(arr > 30 | dep > 30)
#' )
chain <- function(..., env = parent.frame()) {
  chain_q(dots(...), env = env)
}

#' @export
#' @rdname chain
chain_q <- function(calls, env = parent.frame()) {
  if (length(calls) == 0) return()
  if (length(calls) == 1) return(eval(calls[[1]], env))
  
  # New environemnt for evalution - inherits from parent frame, and 
  # contains unusually named (to avoid conflicts) variable to represent
  # result of previous computation
  e <- new.env(parent = env)
  e$`__prev` <- eval(calls[[1]], env)
  
  for(call in calls[-1]) {
    new_call <- as.call(c(call[[1]], quote(`__prev`), as.list(call[-1])))
    e$`__prev` <- eval(new_call, e)
  }
  
  e$`__prev`  
}

#' @export
#' @rdname chain
"%.%" <- function(x, y) {
  chain_q(list(substitute(x), substitute(y)), env = parent.frame())
}
