group_keys_impl <- function(.data) {
  select(group_data(.data), -last_col())
}

#' The grouping keys
#'
#' @family grouping functions
#'
#' @inheritParams group_split
#'
#' @section grouped data frames
#'
#'  The primary use case for [group_keys()] is with already grouped data frames,
#'  typically a result of [group_by()]. In this case [group_keys()] only uses
#'  the first argument, the grouped tibble, and warns when `...` is used.
#'
#' @section ungrouped data frames
#'
#'  When used on ungrouped data frames, [group_keys()] forwards the `...` to
#'  [group_by()] before retrieving the keys, therefore the `...` are subject to the data mask.
#'
#' @return A tibble with one row per group (even if empty)
#' and one column per grouping variable.
#'
#' @examples
#'
#' #----- primary interface, on grouped tibble
#' iris %>%
#'   group_by(Species) %>%
#'   group_keys()
#'
#' # this may be useful if the grouped data needs further
#' # manipulation before retrieving the keys
#' iris %>%
#'   group_by(Species) %>%
#'   filter(Sepal.Length > mean(Sepal.Length)) %>%
#'   group_keys()
#'
#' #----- Specifying the grouping, subject to the data mask
#' iris %>%
#'   group_keys(Species)
#'
#' @export
group_keys <- function(.data, ...) {
  UseMethod("group_keys")
}

#' @export
group_keys.data.frame <- function(.data, ...){
  group_keys_impl(group_by(.data, ...))
}

#' @export
group_keys.grouped_df <- function(.data, ...) {
  if (dots_n(...)) {
    warn("... is ignored in group_keys(<grouped_df>), please use group_by(..., add = TRUE) %>% group_split()")
  }
  group_keys_impl(.data)
}

#' @export
group_keys.rowwise_df <- function(.data, ...) {
  abort("group_keys() is not meaningful for row wise data frames")
}
