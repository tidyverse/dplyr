
#' List of rows of each group
#'
#' @param .data a tibble
#'
#' @return a list of integer vectors, giving the indices of the rows of each group
#'
#' @examples
#' df <- tibble(x = c(1,1,2,2))
#' rows(df)
#' rows(group_by(df, x))
#' @export
rows <- function(.data) {
  UseMethod("rows")
}

#' @export
rows.data.frame <- function(.data) {
  rows_impl(.data)
}
