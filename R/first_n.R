
#' Get the first n rows of each group
#'
#' @param .data a tbl
#' @param n if positive, the `n` first rows are kept, otherwise the `-n` last rows are dropped
#'
#' @examples
#'
#' first_n(iris, 3)
#'
#' # the first 4 rows of each group
#' iris %>%
#'   group_by(Species) %>%
#'   first_n(4)
#'
#' # Keep n() - 49 rows for each group
#' iris %>%
#'   group_by(Species) %>%
#'   first_n(-49)
#' @export
first_n <- function(.data, n) {
  first_n_impl(.data, n)
}
