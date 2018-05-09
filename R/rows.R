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
  list(seq(0, nrow(.data) - 1))
}

#' @export
rows.rowwise_df <- function(.data) {
  as.list(seq(0, nrow(.data) - 1))
}

#' @export
rows.grouped_df <- function(.data) {
  attr(.data, "groups")$.rows
}

#' grouping data
#'
#' @param .data a tibble
#'
#' @return a tibble with one row per group. The last column, always called `.rows` is a list of integer vectors
#' indicating the rows for each group. If `.data` is a grouped data frame the first columns
#' are the grouping variables.
#'
#' @examples
#' df <- tibble(x = c(1,1,2,2))
#'
#' # one row
#' group_data(df)
#'
#' # 2 rows, one for each group
#' group_by(df,x) %>% group_data()
#'
#' # n rows
#' rowwise(df) %>% group_data()
#' @export
group_data <- function(.data) {
  UseMethod("group_data")
}

#' @export
group_data.data.frame <- function(.data) {
  n <- nrow(.data)
  tibble(.rows=list(seq(0, n-1L)))
}

#' @export
group_data.rowwise_df <- function(.data) {
  n <- nrow(.data)
  tibble(.rows=as.list(seq(0, n-1L)))
}

#' @export
group_data.grouped_df <- function(.data) {
  attr(.data, "groups")
}
