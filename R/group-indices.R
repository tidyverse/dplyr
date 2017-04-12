#' Group id.
#'
#' Generate a unique id for each group
#'
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
  group_indices_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
group_indices_ <- function(.data, ..., .dots = list()) {
  UseMethod("group_indices_")
}

#' @export
group_indices.data.frame <- function(.data, ...) {
  dots <- quos(...)
  if (length(dots) == 0L) {
    return(rep(1L, nrow(.data)))
  }
  grouped_indices_grouped_df_impl(group_by(.data, !!! dots))
}
#' @export
group_indices_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_indices(.data, !!! dots)
}

#' @export
group_indices.grouped_df <- function(.data, ...) {
  if (length(list(...))) {
    warn("group_indices_.grouped_df ignores extra arguments")
  }
  grouped_indices_grouped_df_impl(.data)
}
#' @export
group_indices_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_indices(.data, !!! dots)
}
