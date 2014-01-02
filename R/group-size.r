#' Calculate the size of each group
#'
#' @param x a grouped tbl
#' @export
#' @examples
#' if (require("hflights")) {
#' group_size(group_by(hflights, Year, Month, DayofMonth))
#' group_size(group_by(hflights, Dest))
#' }
group_size <- function(x) UseMethod("group_size")
