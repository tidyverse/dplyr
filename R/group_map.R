
#' Apply a function to each group
#'
#' \badgeexperimental
#'
#' `group_map()` and `group_walk()` are purrr-style functions that can
#' be used to iterate on grouped tibbles.
#'
#' Each conceptual group of the data frame is exposed to the function `.f` with two pieces of information:
#'
#'   - The subset of the data for the group, exposed as `.x`.
#'   - The key, a tibble with exactly one row and columns for each grouping variable, exposed as `.y`
#'
#' @family grouping functions
#'
#' @param .tbl A grouped tibble
#' @param .f A function or formula to apply to each group. It must return a data frame.
#'
#'   If a __function__, it is used as is. It should have at least 2 formal arguments.
#'
#'   If a __formula__, e.g. `~ head(.x)`, it is converted to a function.
#'
#'   In the formula, you can use
#'
#'   -  `.` or `.x` to refer to the subset of rows of `.tbl`
#'   for the given group
#'
#'   - `.y` to refer to the key, a one row tibble with one column per grouping variable
#'   that identifies the group
#'
#' @param ... Additional arguments passed on to `.f`
#' @param keep Should `.x` contain the grouping variables
#'
#' @return
#'  - `group_map()` combines the data frames returned by `.f`
#'  - `group_walk()` calls `.f` for side effects, and eventually invisibly returns `.tbl` unchanged
#'
#' @seealso [group_split()] and [group_keys()]
#'
#' @examples
#' mtcars %>%
#'   group_by(cyl) %>%
#'   group_map(~ head(.x, 2L))
#'
#' iris %>%
#'   group_by(Species) %>%
#'   filter(Species == "setosa") %>%
#'   group_map(~ tally(.x))
#'
#' @export
group_map <- function(.tbl, .f, ..., keep = FALSE) {
  UseMethod("group_map")
}

#' @export
group_map.grouped_df <- function(.tbl, .f, ..., keep = FALSE) {
  .f <- rlang::as_function(.f)

  # call the function on each group
  chunks <- group_split(.tbl, keep = isTRUE(keep))
  keys  <- group_keys(.tbl)
  group_keys <- map(seq_len(nrow(keys)), function(i) keys[i, , drop = FALSE])
  result_tibbles <- map2(chunks, group_keys, function(.x, .y){
    res <- .f(.x, .y, ...)
    bind_cols(.y[rep(1L, nrow(res)), , drop = FALSE], res)
  })

  # recalculates .rows based on the number of rows on each tibble
  .rows <- vector(mode = "list", length = length(result_tibbles))
  k <- 1L
  for (i in seq_along(result_tibbles)) {
    n <- nrow(result_tibbles[[i]])
    .rows[[i]] <- seq2(k, k + n - 1L)
    k <- k + n
  }

  # structure the result as a grouped data frame
  new_grouped_df(
    bind_rows(!!!result_tibbles),
    groups = tibble::add_column(keys, ".rows" := .rows)
  )
}

#' @export
#' @rdname group_map
group_walk <- function(.tbl, .f, ...) {
  UseMethod("group_walk")
}

#' @export
group_walk.grouped_df <- function(.tbl, .f, ...) {
  .f <- rlang::as_function(.f)

  # call the function on each group
  chunks <- group_split(.tbl, keep = isTRUE(keep))
  keys  <- group_keys(.tbl)
  group_keys <- map(seq_len(nrow(keys)), function(i) keys[i, , drop = FALSE])
  walk2(chunks, group_keys, .f)
  invisible(.tbl)
}
