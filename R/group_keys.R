group_keys_impl <- function(.data) {
  structure(
    select(group_data(.data), -last_col()),
    .drop = NULL
  )
}

#' @rdname group_split
#' @export
group_keys <- function(.tbl, ...) {
  UseMethod("group_keys")
}

#' @export
group_keys.data.frame <- function(.tbl, ...){
  group_keys_impl(group_by(.tbl, ...))
}

#' @export
group_keys.grouped_df <- function(.tbl, ...) {
  if (dots_n(...)) {
    warn("... is ignored in group_keys(<grouped_df>), please use group_by(..., add = TRUE) %>% group_keys()")
  }
  group_keys_impl(.tbl)
}

#' @export
group_keys.rowwise_df <- function(.tbl, ...) {
  new_tibble(list(), nrow = nrow(.tbl))
}
