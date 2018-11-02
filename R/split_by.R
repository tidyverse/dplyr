#' Split data frame by groups
#'
#' @param .data A tbl
#' @param ... See [group_by()]
#'
#' @return a list of tibbles. Each tibble contains the rows of `.data` for the associated group.
#'
#' @section Scoped grouping:
#'
#' The scoped variants [split_by_at()] and [split_by_if()] make it easy to split by a
#' selection of varibles.
#'
#' @examples
#' iris %>%
#'   split_by(Species)
#'
#' iris %>%
#'   split_by_at(vars("Species"))
#'
#' iris %>%
#'   split_by_if(is.factor)
#' @export
split_by <- function(.data, ...) {
  split_by_impl(group_by(.data, ...), environment())
}

#' @rdname split_by
#' @export
split_by_at <- function(.data, ...){
  split_by_impl(group_by_at(.data, ...), environment())
}

#' @rdname split_by
#' @export
split_by_if <- function(.data, ...){
  split_by_impl(group_by_if(.data, ...), environment())
}
