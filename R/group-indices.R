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
#' @rdname se-deprecated
group_indices_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "group_indices_() is deprecated. ",
    "Please use group_indices() instead"
  ))

  UseMethod("group_indices_")
}

#' @export
group_indices.data.frame <- function(.data, ..., .drop = TRUE) {
  dots <- enquos(...)
  if (length(dots) == 0L) {
    return(rep(1L, nrow(.data)))
  }
  grouped_indices_grouped_df_impl(group_by(.data, !!!dots, .drop = .drop))
}
#' @export
group_indices_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_indices(.data, !!!dots)
}

#' @export
group_indices.rowwise_df <- function(.data, ...) {
  if (dots_n(...)) {
    warn("group_indices_.rowwise_df ignores extra arguments")
  }
  seq_len(nrow(.data))
}
#' @export
group_indices_.rowwise_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_indices(.data, !!!dots)
}

#' @importFrom rlang dots_n
#' @export
group_indices.grouped_df <- function(.data, ...) {
  if (dots_n(...)) {
    warn("group_indices_.grouped_df ignores extra arguments")
  }
  grouped_indices_grouped_df_impl(.data)
}
#' @export
group_indices_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_indices(.data, !!!dots)
}
