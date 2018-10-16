
#' Get the first or last n rows of each group
#'
#' @param .data a tbl
#' @param n if positive, the `n` first rows are kept, otherwise the `-n` last rows are dropped
#'
#' @examples
#'
#' first_n(iris, 3)
#' last_n(iris, 2)
#'
#' # the first 4 rows of each group
#' iris %>%
#'   group_by(Species) %>%
#'   first_n(4)
#'
#' # the last 2 rows of each group
#' iris %>%
#'   group_by(Species) %>%
#'   last_n(2)
#'
#' # Keep n() - 49 rows for each group
#' iris %>%
#'   group_by(Species) %>%
#'   first_n(-49)
#'
#' @export
first_n <- function(.data, n) {
  first_n_impl(.data, n)
}

#' @rdname first_n
#' @export
last_n <- function(.data, n) {
  last_n_impl(.data, n)
}
