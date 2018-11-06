
#' Split data frame by groups
#'
#' @param .data A tbl
#' @param ... See [group_by()]
#' @param .add See [group_by()]
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
#'
#' # chop an already grouped data frame
#' iris %>%
#'   group_by(Species) %>%
#'   chop()
#'
#' @export
split_by <- function(.data, ..., .add = TRUE) {
  split_by_impl(group_by(.data, ..., add = .add), environment())
}

#' @rdname split_by
#' @export
split_by_at <- function(.data, ..., .add = TRUE){
  split_by_impl(group_by_at(.data, ..., .add = .add), environment())
}

#' @rdname split_by
#' @export
split_by_if <- function(.data, ..., .add = TRUE){
  split_by_impl(group_by_if(.data, ..., .add = .add), environment())
}

#' @export
split.grouped_df <- function(x, f, drop = FALSE, ...) {
  if(nargs() != 1) {
    abort("split() on a grouped tibble is only supported without arguments, consider split_by()")
  }
  split_by_impl(x, environment())
}

#' @export
split.tbl_df <- function(x, f, drop = FALSE, ...) {
  abort("split() not supported for tibbles, you probably need split_by()")
}

#' @export
split.rowwise_df <- function(x, f, drop = FALSE, ...) {
  if(nargs() != 1) {
    abort("split() on a rowwise tibble is only supported without arguments, consider split_by()")
  }
  split_rowwise(x, environment())
}
