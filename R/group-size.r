#' Calculate group sizes.
#'
#' @family grouping functions
#' @param x a grouped tbl
#' @export
#' @examples
#' if (require("nycflights13")) {
#'
#' by_day <- flights %>% group_by(year, month, day)
#' n_groups(by_day)
#' group_size(by_day)
#'
#' by_dest <- flights %>% group_by(dest)
#' n_groups(by_dest)
#' group_size(by_dest)
#' }
#' @keywords internal
group_size <- function(x) UseMethod("group_size")

#' @export
group_size.data.frame <- function(x) nrow(x)


#' @export
#' @rdname group_size
n_groups <- function(x) UseMethod("n_groups")

#' @export
n_groups.data.frame <- function(x) 1L
