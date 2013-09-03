#' Calculate the size of each group
#'
#' @param x a grouped tbl
#' @export
#' @examples
#' data("baseball", package = "plyr")
#' players_df <- group_by(baseball, id)
#' group_size(players_df)
group_size <- function(x) UseMethod("group_size")
