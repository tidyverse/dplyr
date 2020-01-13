row_slice <- function(x, i, ...) {
  if (!is.numeric(i) && !is.logical(i)) {
    abort("`i` must be an numeric or logical vector")
  }

  UseMethod("row_slice")
}

#' @export
row_slice.data.frame <- function(x, i, ...) {
  vec_slice(x, i)
}

#' @export
row_slice.grouped_df <- function(x, i, ..., preserve = FALSE) {
  data <- vec_slice(x, i)

  groups <- group_data(x)
  new_id <- vec_slice(group_indices(x), i)
  new_grps <- vec_group_pos(new_id)

  rows <- rep(list_of(integer()), length = nrow(groups))
  rows[new_grps$key] <- new_grps$pos
  groups$.rows <- rows
  if (!preserve && isTRUE(attr(groups, ".drop"))) {
    groups <- group_data_trim(groups)
  }

  new_grouped_df(data, groups)
}
