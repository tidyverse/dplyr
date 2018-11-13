#' Split data frame by groups
#'
#' @family grouping functions
#'
#' @param .data A tbl
#' @param ... See [group_by()], [group_by_at()] or [group_by_if()]
#'
#' @return a list of tibbles. Each tibble contains the rows of `.data` for the associated group.
#'
#' @section Scoped grouping:
#'
#' The scoped variants [group_split_at()] and [group_split_if()] make it easy to split by a
#' selection of variables.
#'
#' @examples
#' iris %>%
#'   group_split(Species)
#'
#' iris %>%
#'   group_split_at(vars("Species"))
#'
#' iris %>%
#'   group_split_if(is.factor)
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
group_split.default <- function(.data, ...){
  abort("unsupported")
}

#' @export
group_split.data.frame <- function(.data, ...){
  group_split_impl(group_by(.data, ...), environment())
}

#' @export
group_split.grouped_df <- function(.data, ...) {
  if (dots_n(...)) {
    warn("... is not supported in group_split(<grouped_df>), consider group_by(..., add = TRUE) %>% group_split()")
  }
  group_split_impl(.data, environment())
}

#' @rdname group_split
#' @export
group_split_at <- function(.data, ...){
  group_split_impl(group_by_at(.data, ...), environment())
}

#' @rdname group_split
#' @export
group_split_if <- function(.data, ...){
  group_split_impl(group_by_if(.data, ...), environment())
}
