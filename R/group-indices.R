#' Group id.
#'
#' Generate a unique id for each group
#'
#' @family grouping functions
#' @keywords internal
#' @seealso [group_by()]
#' @param .data a tbl
#' @inheritParams group_by
#' @inheritParams filter
#' @export
#' @examples
#' group_indices(mtcars, cyl)
group_indices <- function(.data, ...) {
  UseMethod("group_indices")
}
#' @export
group_indices.default <- function(.data, ...) {
  if (missing(.data)) {
    rep.int(from_context("..group_number"), from_context("..group_size"))
  } else {
    group_indices_(.data, .dots = compat_as_lazy_dots(...))
  }
}

#' @export
group_indices.rowwise_df <- function(.data, ...) {
  if (dots_n(...)) {
    warn("group_indices_.rowwise_df ignores extra arguments")
  }
  seq_len(nrow(.data))
}

#' @importFrom rlang dots_n
#' @export
group_indices.grouped_df <- function(.data, ...) {
  if (dots_n(...)) {
    warn("group_indices_.grouped_df ignores extra arguments")
  }
  .Call(`dplyr_group_indices`, .data, nrow(.data))
}
