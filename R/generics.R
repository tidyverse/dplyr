#' Extending dplyr
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' These three functions, along with `names<-` and 1d `[`, provide a minimal
#' interface for extending dplyr to work with a new subclass of data frame. They
#' are experimental, and a stop-gap measure until more general tools become
#' available in vctrs, but may still be useful in the short-term if you have a
#' data frame class that you want to work with dplyr.
#'
#' * `arrange()`, `filter()`, `slice()`, `semi_join()`, and `anti_join()`
#'   work by generating a vector of column indices, and then subsetting
#'   with `dplyr_row_slice()`.
#'
#' * `mutate()` generates a list of new column value (using `NULL` to indicate
#'   when columns should be deleted), then passes that to `dplyr_col_modify()`.
#'   `transmute()` does the same then uses 1d `[` to select the columns.
#'
#' * `summarise()` works similarly to `mutate()` but the data modified by
#'   `dplyr_col_modify()` comes from `group_data()`.
#'
#' * `select()` uses 1d `[` to select columns, then `names<-` to rename them.
#'   `rename()` just uses `names<-`.
#'
#' * `inner_join()`, `left_join()`, `right_join()`, and `full_join()`
#'   coerces `x` to a tibble, modify the rows, then use `dplyr_df_restore()`
#'   to convert back to the same type as `x`.
#'
#' * `distinct()` does a `mutate()` if any expressions are present, then
#'   use 1d`[`
#'
#' Note that `group_by()` and `ungroup()` don't use any of these tools and
#' you'll need to provide methods directly.
#'
#' @name dplyr_extending
NULL

#' @export
#' @rdname dplyr_extending
dplyr_row_slice <- function(data, i, ...) {
  if (!is.numeric(i) && !is.logical(i)) {
    abort("`i` must be an numeric or logical vector")
  }

  UseMethod("dplyr_row_slice")
}

#' @export
dplyr_row_slice.data.frame <- function(data, i, ...) {
  vec_slice(data, i)
}

#' @export
dplyr_row_slice.grouped_df <- function(data, i, ..., preserve = FALSE) {
  out <- vec_slice(data, i)

  # Index into group_indices, then use that to restore the grouping structure
  groups <- group_data(data)
  new_id <- vec_slice(group_indices(data), i)
  new_grps <- vec_group_pos(new_id)

  rows <- rep(list_of(integer()), length = nrow(groups))
  rows[new_grps$key] <- new_grps$pos
  groups$.rows <- rows
  if (!preserve && isTRUE(attr(groups, ".drop"))) {
    groups <- group_data_trim(groups)
  }

  new_grouped_df(out, groups)
}

#' @export
#' @rdname dplyr_extending
dplyr_col_modify <- function(data, cols) {
  UseMethod("dplyr_col_modify")
}

#' @export
dplyr_col_modify.data.frame <- function(data, cols) {
  data[names(cols)] <- cols
  data
}

#' @export
dplyr_col_modify.grouped_df <- function(data, cols) {
  out <- dplyr_col_modify(as_tibble(data), cols)

  if (any(names(cols) %in% group_vars(data))) {
    # regroup
    grouped_df(out, group_vars(data), drop = group_by_drop_default(data))
  } else {
    new_grouped_df(out, group_data(data))
  }
}

#' @export
#' @rdname dplyr_extending
dplyr_df_restore <- function(data, old) {
  if (!is_tibble(data)) {
    abort("`new` must be a tibble")
  }

  UseMethod("dplyr_df_restore", old)
}

#' @export
dplyr_df_restore.data.frame <- function(data, old) {
  attr_old <- attributes(old)
  attr_new <- attributes(data)

  to_copy <- setdiff(names(attr_old), c("row.names", "names", ".drop"))
  attr_new[to_copy] <- attr_old[to_copy]

  attributes(data) <- attr_new
  data
}

#' @export
dplyr_df_restore.grouped_df <- function(data, old) {
  group_vars <- intersect(group_vars(old), names(data))
  grouped_df(data, group_vars, drop = group_by_drop_default(old))
}
