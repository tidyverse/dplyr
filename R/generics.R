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


# new should be a tibble?
# TODO: test that's true
df_restore <- function(old, new) {
  UseMethod("df_restore")
}

df_restore.data.frame <- function(old, new) {
  attr_old <- attributes(old)
  attr_new <- attributes(new)

  to_copy <- setdiff(names(attr_old), c("row.names", "names", ".drop"))
  attr_new[to_copy] <- attr_old[to_copy]

  attributes(new) <- attr_new
  new
}

df_restore.grouped_df <- function(old, new) {
  group_vars <- intersect(group_vars(old), names(new))
  grouped_df(new, group_vars, drop = group_by_drop_default(old))
}
