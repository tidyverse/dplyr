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

  # new_groups$.rows <- new_list_of(
  #   .Call(`dplyr_bundle_rows`, old_rows),
  #   ptype = integer(),
  #   class = "bundled_indices"
  # )
  breaks <- cumsum(c(1L, list_sizes(old_rows)))
  start <- breaks[-length(breaks)]
  end <- breaks[-1] - 1L
  new_groups$.rows <- new_list_of(
    map2(start, end, seq2),
    ptype = integer(),
    class = "bundled_indices"
  )

  new_grouped_df(
    vec_slice(.data, indices),
    groups = new_groups,
    class = "bundled_df"
  )
}
