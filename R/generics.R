#' Extending dplyr
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' These three functions, along with `names<-` and 1d numeric `[`
#' (i.e. `x[loc]`), provide a minimal interface for extending dplyr to work
#' with a new subclass of data frame. `dplyr_rows_slice()` and
#' `dplyr_cols_modify()` represented specialised uses of `[`; their focus
#' makes them much easier to implement. `dplyr_reconstruct()` is a fall-back
#' for verbs that can't be implemented in terms of simpler row/col operation.
#'
#' These functions are experimental and are stop-gap measure until more general
#' tools become available in vctrs, but may still be useful in the short-term
#' if you have a data frame class that you want to work with dplyr.
#'
#' # Current usage
#'
#' * `arrange()`, `filter()`, `slice()`, `semi_join()`, and `anti_join()`
#'   work by generating a vector of row indices, and then subsetting
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
#'   `rename()` just uses `names<-`. `relocate()` just uses 1d `[`.
#'
#' * `inner_join()`, `left_join()`, `right_join()`, and `full_join()`
#'   coerces `x` to a tibble, modify the rows, then uses `dplyr_reconstruct()`
#'   to convert back to the same type as `x`.
#'
#' * `nest_join()` uses `dplyr_col_modify()` to cast the key variables to
#'   common type and add the nested-df that `y` becomes.
#'
#' * `distinct()` does a `mutate()` if any expressions are present, then
#'   uses 1d `[` to select variables to keep, then `dplyr_row_slice()` to
#'   select distinct rows.
#'
#' Note that `group_by()` and `ungroup()` don't use any these generics and
#' you'll need to provide methods directly.
#'
#' @keywords internal
#' @param data A tibble. We use tibbles because they avoid some inconsistent
#'    subset-assignment use cases
#' @name dplyr_extending
NULL

#' @export
#' @rdname dplyr_extending
#' @param i A numeric or logical vector that indexes the rows of `.data`.
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
  new_grps <- vec_group_loc(new_id)

  rows <- rep(list_of(integer()), length = nrow(groups))
  rows[new_grps$key] <- new_grps$loc
  groups$.rows <- rows
  if (!preserve && isTRUE(attr(groups, ".drop"))) {
    groups <- group_data_trim(groups)
  }

  new_grouped_df(out, groups)
}

#' @export
dplyr_row_slice.rowwise_df <- function(data, i, ..., preserve = FALSE) {
  out <- vec_slice(data, i)
  group_data <- vec_slice(group_keys(data), i)
  new_rowwise_df(out, group_data)
}

#' @export
#' @rdname dplyr_extending
#' @param cols A named list used modify columns. A `NULL` value should remove
#'   an existing column.
dplyr_col_modify <- function(data, cols) {
  UseMethod("dplyr_col_modify")
}

#' @export
dplyr_col_modify.data.frame <- function(data, cols) {
  # Implement from first principles to avoiding edge cases in [.data.frame
  # and [.tibble in 2.1.3 and earlier

  # Apply tidyverse recycling rules
  cols <- lapply(cols, vec_recycle, size = nrow(data))

  names(cols) <- as_utf8_character(names2(cols))
  names(data) <- as_utf8_character(names2(data))

  out <- vec_data(data)
  for (i in seq_along(cols)) {
    nm <- names(cols)[[i]]
    out[[nm]] <- cols[[i]]
  }

  # Restore attributes (apart from names)
  attr <- attributes(data)
  attr$names <- names(out)
  attr$row.names <- .row_names_info(data, 0L)
  attributes(out) <- attr

  out
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
dplyr_col_modify.rowwise_df <- function(data, cols) {
  out <- dplyr_col_modify(as_tibble(data), cols)
  rowwise_df(out, group_vars(data))
}

#' @param template Template to use for restoring attributes
#' @export
#' @rdname dplyr_extending
dplyr_reconstruct <- function(data, template) {
  if (!is_tibble(data)) {
    abort("`new` must be a tibble")
  }

  UseMethod("dplyr_reconstruct", template)
}

#' @export
dplyr_reconstruct.data.frame <- function(data, template) {
  attr_old <- attributes(template)
  attr_new <- attributes(data)

  to_copy <- setdiff(names(attr_old), c("class", "row.names", "names", ".drop"))
  attr_new[to_copy] <- attr_old[to_copy]

  # `new_data_frame()` will add the `"data.frame"` class
  class <- setdiff(class(template), "data.frame")
  attr_new[["class"]] <- NULL

  size <- vec_size(data)

  data <- exec(new_data_frame, x = data, n = size, class = class, !!! attr_new)

  data
}

#' @export
dplyr_reconstruct.grouped_df <- function(data, template) {
  group_vars <- intersect(group_vars(template), names(data))
  grouped_df(data, group_vars, drop = group_by_drop_default(template))
}

#' @export
dplyr_reconstruct.rowwise <- function(data, template) {
  group_vars <- intersect(group_vars(template), names(data))
  rowwise(grouped_df(data, group_vars))
}
