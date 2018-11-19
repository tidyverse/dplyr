#' Split data frame by groups
#'
#' @family grouping functions
#'
#' [group_split()] works like [base::split()] but
#' - it uses the grouping structure from [group_by()]
#' - it does not name the elements of the list based on the grouping as this typically
#'   loses information and is confusing.
#'
#' @section grouped data frames
#'
#'  The primary use case for [group_split()] is with already grouped data frames,
#'  typically a result of [group_by()]. In this case [group_split()] only uses
#'  the first argument, the grouped tibble, and warns when `...` is used.
#'
#' @section ungrouped data frames
#'
#'  When used on ungrouped data frames, [group_split()] forwards the `...` to
#'  [group_by()] before the split, therefore the `...` are subject to the data mask.
#'
#' @section rowwise data frames
#'
#'  A list of one-row tibbles is returned, and the `...` are ignored and warned against
#'
#' @param .tbl A tbl
#' @param ... See [group_by()], [group_by_at()] or [group_by_if()]
#'
#' @return a list of tibbles. Each tibble contains the rows of `.tbl` for the associated group.
#'
#'  The returned object does not contain grouping information, but it can be paired with
#'  [group_keys()] to get that information, each row of the data frame returned
#'  by [group_keys()] is associated to one element of the result of [group_split()].
#'
#' @examples
#'
#' # use case 1 : on an already grouped tibble
#' iris %>%
#'   group_by(Species) %>%
#'   group_split()
#'
#' # this can be useful if the grouped data has been altered before the split
#' iris %>%
#'   group_by(Species) %>%
#'   filter(Sepal.Length > 6) %>%
#'   group_split()
#'
#' # use case 2: using a group_by() grouping specification
#' iris %>%
#'   group_split(Species)
#'
#' @export
group_split <- function(.tbl, ...) {
  UseMethod("group_split")
}

#' @export
group_split.data.frame <- function(.tbl, ...){
  group_split_impl(group_by(.tbl, ...), environment())
}

#' @export
group_split.rowwise_df <- function(.tbl, ...) {
  if (dots_n(...)) {
    warn("... is ignored in group_split(<grouped_df>), please use group_by(..., add = TRUE) %>% group_split()")
  }
  n <- nrow(.tbl)
  map(seq_len(n), function(i) structure(.tbl[i, ], class = c("tbl_df", "tbl", "data.frame")))
}

#' @export
group_split.grouped_df <- function(.tbl, ...) {
  if (dots_n(...)) {
    warn("... is ignored in group_split(<grouped_df>), please use group_by(..., add = TRUE) %>% group_split()")
  }
  group_split_impl(.tbl, environment())
}
