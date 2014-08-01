#' Calculate group sizes.
#'
#'
#'
#' @param x a grouped tbl
#' @export
#' @examples
#' data("hflights", package = "hflights")
#'
#' by_day <- hflights %>% group_by(Year, Month, DayofMonth)
#' n_groups(by_day)
#' group_size(by_day)
#'
#' by_dest <- hflights %>% group_by(Dest)
#' n_groups(by_dest)
#' group_size(by_dest)
group_size <- function(x) UseMethod("group_size")

#' @export
#' @rdname group_size
n_groups <- function(x) UseMethod("n_groups")
