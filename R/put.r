#' Assign to a name after a chain of data manipulations
#'
#' Alias for the base function \code{\link{assign}}, which makes it easier to
#' assign the output of a chain of data manipulations.
#'
#' @param x a value to be assigned to \code{name}.
#' @param name an unquoted variable name.
#' @param where where to do the assignment. Default is the environment in which
#'   the chain of manipulations is called. Passed to \code{pos} argument of
#'   \code{\link{assign}}.
#'
#' @return The function is invoked for its side effect, which is to assign
#'   \code{x} to the variable \code{name}. \code{x} is then returned.
#'
#' @examples
#' mtcars %>%
#' group_by(cyl) %>%
#' summarise_each(funs(mean), drat, wt, qsec) %>%
#' put(car_cyls)
#'
#' print(car_cyls)
#'
#' # It is also possible to assign in the middle of a chain.
#' mtcars %>%
#' group_by(cyl) %>%
#' put(cars_by_cyl) %>%
#' summarise_each(funs(mean, sd), drat, wt, qsec) %>%
#' right_join(cars_by_cyl) %>%
#' put(mtcars_aug)
#' ls()
#'
#' print(cars_by_cyl)
#' print(mtcars_aug)
#'
#' @export

put <- function(x, name, where = NULL) {
  name_string <- deparse(substitute(name))
  if (is.null(where)) {
    sys_calls <- sys.calls()
    put_calls <- grepl("\\<put\\(", sys_calls) & !grepl("\\<put\\(\\.",sys_calls)
    where <- sys.frame(max(which(put_calls)) - 1)
  }
  assign(name_string, value = x, pos = where)
}
