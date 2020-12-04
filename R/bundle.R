#' @export
bundle <- function(.data) {
  UseMethod("bundle")
}

#' @export
bundle.default <- function(.data) {
  .data
}

#' @export
bundle.grouped_df <- function(.data) {
  # reorder rows so that groups are all contiguous
  old_rows <- group_rows(.data)
  indices <- vec_c(!!!old_rows, .ptype = integer())

  # adapt .rows
  new_groups <- attr(.data, "groups")

  new_groups$.rows <- new_list_of(
    .Call(`dplyr_bundle_rows`, old_rows),
    ptype = integer(),
    class = "bundled_indices"
  )

  new_grouped_df(
    vec_slice(.data, indices),
    groups = new_groups,
    class = "bundled_df"
  )
}

dplyr_vec_chop <- function(x, indices) {
  .Call(`_dplyr_vec_chop`, x, indices)
}
