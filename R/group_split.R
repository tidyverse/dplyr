#' Split data frame by groups
#'
#' @family grouping functions
#'
#' [group_split()] works like [base::split()] but
#' - it uses the grouping structure
#'   from [group_by()]. In particular, groups may be empty and lead to tibbles with
#'   0 rows.
#' - it does not name the elements of the list based on the grouping as this typically
#'   loses information and is confusing.
#'
#'  The object that results from [group_split()] does not contain grouping
#'  information, but it can be paired with [group_keys()] to get that information,
#'  each row of the data frame returned by [group_keys()] is associated to one
#'  element of the result of [group_split()].
#'
#' @param .data A tbl
#' @param ... See [group_by()], [group_by_at()] or [group_by_if()]
#'
#' @return a list of tibbles. Each tibble contains the rows of `.data` for the associated group.
#'
#' @examples
#' iris %>%
#'   group_split(Species)
#'
#' # group_split() an already grouped data frame
#' iris %>%
#'   group_by(Species) %>%
#'   group_split()
#'
#' # this can be useful if the grouped data has been altered
#' # before the split
#' iris %>%
#'   group_by(Species) %>%
#'   filter(Sepal.Length > 6) %>%
#'   group_split()
#'
#' @export
group_split <- function(.data, ...) {
  UseMethod("group_split")
}

#' @export
group_split.data.frame <- function(.data, ...){
  group_split_impl(group_by(.data, ...), environment())
}

#' @export
group_split.rowwise_df <- function(.data, ...) {
  n <- nrow(.data)
  map(seq_len(n), function(i) structure(.data[i, ], class = c("tbl_df", "tbl", "data.frame")))
}

#' @export
group_split.grouped_df <- function(.data, ...) {
  if (dots_n(...)) {
    warn("... is ignored in group_split(<grouped_df>), please use group_by(..., add = TRUE) %>% group_split()")
  }
  group_split_impl(.data, environment())
}

group_map <- function(.tbl, .f, ..., env = caller_env()) {
  .f <- rlang::as_function(.f, env = env)
  .datas <- group_split(.tbl)
  .keys  <- group_split(rowwise(select(group_data(.tbl), - last_col())))

  bind_rows(map2(.datas, .keys, .f, ...))
}
