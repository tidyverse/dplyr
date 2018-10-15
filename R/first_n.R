
#' Get the first n rows of each group
#'
#' @param .data a tbl
#' @param n How many rows
#'
#' @examples
#'
#' first_n(iris, 3)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   first_n(4)
#'
#' @export
first_n <- function(.data, n) {
  first_n_impl(.data, n)
}
