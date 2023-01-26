#' Split data frame by groups
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' [group_split()] works like [base::split()] but:
#'
#' - It uses the grouping structure from [group_by()] and therefore is subject
#'   to the data mask
#'
#' - It does not name the elements of the list based on the grouping as this
#'   only works well for a single character grouping variable. Instead,
#'   use [group_keys()] to access a data frame that defines the groups.
#'
#' `group_split()` is primarily designed to work with grouped data frames.
#' You can pass `...` to group and split an ungrouped data frame, but this
#' is generally not very useful as you want have easy access to the group
#' metadata.
#'
#' @section Lifecycle:
#' `group_split()` is not stable because you can achieve very similar results by
#' manipulating the nested column returned from
#' [`tidyr::nest(.by =)`][tidyr::nest()]. That also retains the group keys all
#' within a single data structure. `group_split()` may be deprecated in the
#' future.
#'
#' @param .tbl A tbl.
#' @param ... If `.tbl` is an ungrouped data frame, a grouping specification,
#'   forwarded to [group_by()].
#' @param .keep Should the grouping columns be kept?
#' @returns A list of tibbles. Each tibble contains the rows of `.tbl` for the
#'   associated group and all the columns, including the grouping variables.
#'   Note that this returns a [list_of][vctrs::list_of()] which is slightly
#'   stricter than a simple list but is useful for representing lists where
#'   every element has the same type.
#' @keywords internal
#' @family grouping functions
#' @export
#' @examples
#' ir <- iris %>% group_by(Species)
#'
#' group_split(ir)
#' group_keys(ir)
group_split <- function(.tbl, ..., .keep = TRUE) {
  lifecycle::signal_stage("experimental", "group_split()")
  UseMethod("group_split")
}

#' @export
group_split.data.frame <- function(.tbl, ..., .keep = TRUE, keep = deprecated()) {
  if (!missing(keep)) {
    lifecycle::deprecate_warn("1.0.0", "group_split(keep = )", "group_split(.keep = )", always = TRUE)
    .keep <- keep
  }
  data <- group_by(.tbl, ...)
  group_split_impl(data, .keep = .keep)
}

#' @export
group_split.rowwise_df <- function(.tbl, ..., .keep = TRUE, keep = deprecated()) {
  if (dots_n(...)) {
    warn("... is ignored in group_split(<rowwise_df>), please use as_tibble() %>% group_split(...)")
  }
  if (!missing(keep)) {
    lifecycle::deprecate_warn("1.0.0", "group_split(keep = )", "group_split(.keep = )", always = TRUE)
    .keep <- keep
  }
  if (!missing(.keep)) {
    warn(".keep is ignored in group_split(<rowwise_df>)")
  }

  group_split_impl(.tbl, .keep = TRUE)
}

#' @export
group_split.grouped_df <- function(.tbl, ..., .keep = TRUE, keep = deprecated()) {
  if (!missing(keep)) {
    lifecycle::deprecate_warn("1.0.0", "group_split(keep = )", "group_split(.keep = )", always = TRUE)
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
