group_keys_impl <- function(.data) {
  select(group_data(.data), -last_col())
}

#' The grouping keys
#'
#' The grouping keys are a tibble with only the grouping information.
#'
#' @param .data A tbl
#' @param ... See [group_by()], [group_by_at()] or [group_by_if()]
#'
#' @return A tibble with one row per group and one column per grouping variable
#'
#' @examples
#' iris %>%
#'   group_keys(Species)
#'
#' iris %>%
#'   group_keys_at(vars("Species"))
#'
#' iris %>%
#'   group_keys_if(is.factor)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   group_keys()
#'
#' @export
group_keys <- function(.data, ...) {
  UseMethod("group_keys")
}

#' @export
group_keys.default <- function(.data, ...){
  abort("unsupported")
}

#' @export
group_keys.data.frame <- function(.data, ...){
  group_keys_impl(group_by(.data, ...))
}

#' @export
group_keys.grouped_df <- function(.data, ...) {
  if (dots_n(...)) {
    warn("... is not supported in group_keys(<grouped_df>), consider group_by(..., add = TRUE) %>% group_split()")
  }
  group_keys_impl(.data)
}

#' @rdname group_keys
#' @export
group_keys_at <- function(.data, ...){
  group_keys_impl(group_by_at(.data, ...))
}

#' @rdname group_keys
#' @export
group_keys_if <- function(.data, ...){
  group_keys_impl(group_by_if(.data, ...))
}
