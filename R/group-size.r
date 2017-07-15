#' Calculate group sizes.
#'
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
#' @rdname group_size
n_groups <- function(x) UseMethod("n_groups")
