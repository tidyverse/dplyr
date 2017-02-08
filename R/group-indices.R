#' Group id.
#'
#' Generate a unique id for each group
#'
#' @seealso [group_by()]
#' @param .data a tbl
#' @inheritParams group_by
#' @inheritParams filter
#' @export
#' @examples
#' group_indices(mtcars, cyl)
group_indices <- function(.data, ...) {
  group_indices_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname group_indices
group_indices_ <- function(.data, ..., .dots) {
  UseMethod("group_indices_")
}

#' @export
group_indices_.data.frame <- function(.data, ..., .dots) {
  .dots <- lazyeval::all_dots(..., .dots)
  if (length(.dots) == 0L) {
    return(rep(1L, nrow(.data)))
  }
  grouped_indices_grouped_df_impl(group_by_(.data, .dots = .dots))
}

#' @export
group_indices_.grouped_df <- function(.data, ..., .dots) {
  if (length(list(...)) || (!missing(.dots) && length(.dots))) {
    warning("group_indices_.grouped_df ignores extra arguments")
  }
  grouped_indices_grouped_df_impl(.data)
}
