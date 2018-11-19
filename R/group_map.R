
#' Apply a function to each group
#'
#' \badgeexperimental
#'
#' @family grouping functions
#'
#' @param .tbl A grouped tibble
#' @param .f A function or formula to apply to each group. It must return a data frame.
#'
#'   If a __function__, it is used as is. It should have at least 2 formal arguments.
#'
#'   If a __formula__, e.g. `~ head(.x)`, it is converted to a function. In the formula,
#'   you can use `.` or `.x` to refer to the subset of rows of `.tbl`
#'   for the given group, and `.y` to refer to the key, a one row tibble that
#'   identify the group
#'
#' @param ... Additional arguments passed on to functions.
#'
#' @return The function specified in `.f` is called on each group, and the data frames
#'         are combined with [bind_rows()]
#'
#' @seealso [group_split()] and [group_keys()]
#'
#' @examples
#' # only using .x
#' group_by(mtcars, cyl) %>%
#'   group_map(~ head(.x, 2L))
#'
#' # using both the key (.y) and the data (.x) for each group
#' mtcars %>%
#'   group_by(cyl) %>%
#'   group_map(~ mutate(.y, mod = list(lm(mpg ~ disp, data = .x))))
#'
#' @export
group_map <- function(.tbl, .f, ...) {
  UseMethod("group_map")
}

#' @export
group_map.grouped_df <- function(.tbl, .f, ...) {
  .f <- rlang::as_function(.f)
  .datas <- group_split(.tbl)
  .keys  <- group_split(rowwise(select(group_data(.tbl), - last_col())))

  bind_rows(map2(.datas, .keys, .f, ...))
}
