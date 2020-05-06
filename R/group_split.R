#' Split data frame by groups
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' [group_split()] works like [base::split()] but
#' - it uses the grouping structure from [group_by()] and therefore is subject to the data mask
#' - it does not name the elements of the list based on the grouping as this typically
#'   loses information and is confusing.
#'
#' [group_keys()] explains the grouping structure, by returning a data frame that has one row
#' per group and one column per grouping variable.
#'
#' @section Grouped data frames:
#'
#' The primary use case for [group_split()] is with already grouped data frames,
#' typically a result of [group_by()]. In this case [group_split()] only uses
#' the first argument, the grouped tibble, and warns when `...` is used.
#'
#' Because some of these groups may be empty, it is best paired with [group_keys()]
#' which identifies the representatives of each grouping variable for the group.
#'
#' @section Ungrouped data frames:
#'
#' When used on ungrouped data frames, [group_split()] and [group_keys()] forwards the `...` to
#' [group_by()] before the split, therefore the `...` are subject to the data mask.
#'
#' Using these functions on an ungrouped data frame only makes sense if you need only one or the
#' other, because otherwise the grouping algorithm is performed each time.
#'
#' @section Rowwise data frames:
#'
#' [group_split()] returns a list of one-row tibbles is returned, and the `...` are ignored and warned against
#'
#' @param .tbl A tbl
#' @param ... Grouping specification, forwarded to [group_by()]
#' @param .keep Should the grouping columns be kept
#' @return
#' - [group_split()] returns a list of tibbles. Each tibble contains the rows of `.tbl` for the associated group and
#'  all the columns, including the grouping variables.
#'
#' - [group_keys()] returns a tibble with one row per group, and one column per grouping variable
#' @family grouping functions
#' @export
#' @examples
#' # ----- use case 1 : on an already grouped tibble
#' ir <- iris %>%
#'   group_by(Species)
#'
#' group_split(ir)
#' group_keys(ir)
#'
#' # this can be useful if the grouped data has been altered before the split
#' ir <- iris %>%
#'   group_by(Species) %>%
#'   filter(Sepal.Length > mean(Sepal.Length))
#'
#' group_split(ir)
#' group_keys(ir)
#'
#' # ----- use case 2: using a group_by() grouping specification
#'
#' # both group_split() and group_keys() have to perform the grouping
#' # so it only makes sense to do this if you only need one or the other
#' iris %>%
#'   group_split(Species)
#'
#' iris %>%
#'   group_keys(Species)
group_split <- function(.tbl, ..., .keep = TRUE) {
  UseMethod("group_split")
}

#' @export
group_split.data.frame <- function(.tbl, ..., .keep = TRUE, keep) {
  if (!missing(keep)) {
    lifecycle::deprecate_warn("1.0.0", "group_split(keep = )", "group_split(.keep = )")
    .keep <- keep
  }
  data <- group_by(.tbl, ...)
  group_split_impl(data, .keep = .keep)
}

#' @export
group_split.rowwise_df <- function(.tbl, ..., .keep = TRUE, keep) {
  if (dots_n(...)) {
    warn("... is ignored in group_split(<rowwise_df>), please use as_tibble() %>% group_split(...)")
  }
  if (!missing(keep)) {
    lifecycle::deprecate_warn("1.0.0", "group_split(keep = )", "group_split(.keep = )")
    .keep <- keep
  }
  if (!missing(.keep)) {
    warn(".keep is ignored in group_split(<rowwise_df>)")
  }

  group_split_impl(.tbl, .keep = TRUE)
}

#' @export
group_split.grouped_df <- function(.tbl, ..., .keep = TRUE, keep) {
  if (!missing(keep)) {
    lifecycle::deprecate_warn("1.0.0", "group_split(keep = )", "group_split(.keep = )")
    .keep <- keep
  }
  if (dots_n(...)) {
    warn("... is ignored in group_split(<grouped_df>), please use group_by(..., .add = TRUE) %>% group_split()")
  }

  group_split_impl(.tbl, .keep = .keep)
}

group_split_impl <- function(data, .keep) {
  out <- ungroup(data)
  indices <- group_rows(data)

  if (!isTRUE(.keep)) {
    remove <- group_vars(data)
    .keep <- names(out)
    .keep <- setdiff(.keep, remove)
    out <- out[.keep]
  }

  dplyr_chop(out, indices)
}

dplyr_chop <- function(data, indices) {
  out <- map(indices, dplyr_row_slice, data = data)
  out <- new_list_of(out, ptype = vec_ptype(data))
  out
}
