#' Trim grouping structure
#'
#' ```
#' \Sexpr[results=rd, stage=render]{mypkg:::lifecycle("experimental")}
#' ```
#'
#' @family grouping functions
#'
#' @description
#' Drop unused levels of all factors that are used as grouping variables,
#' then recalculates the grouping structure.
#'
#' `group_trim()` is particularly useful after a [filter()] that is intended
#' to select a subset of groups.
#'
#' @param .tbl A [grouped data frame][grouped_df()]
#'
#' @return A [grouped data frame][grouped_df()]
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   filter(Species == "setosa") %>%
#'   group_trim()
#'
#' @export
group_trim <- function(.tbl) {
  UseMethod("group_trim")
}

#' @export
group_trim.data.frame <- function(.tbl) {
  .tbl
}

#' @export
group_trim.grouped_df <- function(.tbl) {
  vars <- group_vars(.tbl)
  ungrouped <- ungroup(.tbl)

  # names of the factors that should be droplevels()'d
  fgroups <- names(select_if(select_at(ungrouped, vars), is.factor))

  # drop levels
  dropped <- mutate_at(ungrouped, fgroups, droplevels)

  # regroup
  group_by_at(dropped, vars)
}
