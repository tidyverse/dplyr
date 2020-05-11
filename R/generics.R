#' Extending dplyr with new data frame subclasses
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' These three functions, along with `names<-` and 1d numeric `[`
#' (i.e. `x[loc]`) methods, provide a minimal interface for extending dplyr
#' to work with new data frame subclasses. This means that for simple cases
#' you should only need to provide a couple of methods, rather than a method
#' for every dplyr verb.
#'
#' These functions are a stop-gap measure until we figure out how to solve
#' the problem more generally, but it's likely that any code you write to
#' implement them will find a home in what comes next.
#'
#' # Basic advice
#'
#' This section gives you basic advice if you want to extend dplyr to work with
#' your custom data frame subclass, and you want the dplyr methods to behave
#' in basically the same way.
#'
#' * If you have data frame attributes that don't depend on the rows or columns
#'   (and should unconditionally be preserved), you don't need to do anything.
#'
#' * If you have __scalar__ attributes that depend on __rows__, implement a
#'   `dplyr_reconstruct()` method. Your method should recompute the attribute
#'   depending on rows now present.
#'
#' * If you have __scalar__ attributes that depend on __columns__, implement a
#'   `dplyr_reconstruct()` method and a 1d `[` method. For example, if your
#'   class requires that certain columns be present, your method should return
#'   a data.frame or tibble when those columns are removed.
#'
#' * If your attributes are __vectorised__ over __rows__, implement a
#'   `dplyr_row_slice()` method. This gives you access to `i` so you can
#'   modify the row attribute accordingly. You'll also need to think carefully
#'   about how to recompute the attribute in `dplyr_reconstruct()`, and
#'   you will need to carefully verify the behaviour of each verb, and provide
#'   additional methods as needed.
#'
#' * If your attributes that are __vectorised__ over __columns__, implement
#'   `dplyr_col_modify()`, 1d `[`, and `names<-` methods. All of these methods
#'   know which columns are being modified, so you can update the column
#'   attribute according. You'll also need to think carefully about how to
#'   recompute the attribute in `dplyr_reconstruct()`, and you will need to
#'   carefully verify the behaviour of each verb, and provide additional
#'   methods as needed.
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
    abort("`i` must be an numeric or logical vector.")
  }

  UseMethod("dplyr_row_slice")
}

#' @export
dplyr_row_slice.data.frame <- function(data, i, ...) {
  dplyr_reconstruct(vec_slice(data, i), data)
}

#' @export
dplyr_row_slice.grouped_df <- function(data, i, ..., preserve = FALSE) {
  out <- vec_slice(as.data.frame(data), i)

  # Index into group_indices, then use that to restore the grouping structure
  groups <- group_data(data)
  new_id <- vec_slice(group_indices(data), i)
  new_grps <- vec_group_loc(new_id)

  rows <- rep(list_of(integer()), length.out = nrow(groups))
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
  # Must be implemented from first principles to avoiding edge cases in
  # [.data.frame and [.tibble (2.1.3 and earlier).

  # Apply tidyverse recycling rules
  cols <- vec_recycle_common(!!!cols, .size = nrow(data))

  out <- vec_data(data)
  attr(out, "row.names") <- .row_names_info(data, 0L)

  nms <- as_utf8_character(names2(cols))
  names(out) <- as_utf8_character(names2(out))

  for (i in seq_along(cols)) {
    nm <- nms[[i]]
    out[[nm]] <- cols[[i]]
  }

  dplyr_reconstruct(out, data)
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
  UseMethod("dplyr_reconstruct", template)
}

#' @export
dplyr_reconstruct.data.frame <- function(data, template) {
  attrs <- attributes(template)
  attrs$names <- names(data)
  attrs$row.names <- .row_names_info(data, type = 0L)

  attributes(data) <- attrs
  data
}

#' @export
dplyr_reconstruct.grouped_df <- function(data, template) {
  group_vars <- group_intersect(template, data)
  grouped_df(data, group_vars, drop = group_by_drop_default(template))
}

#' @export
dplyr_reconstruct.rowwise_df <- function(data, template) {
  group_vars <- group_intersect(template, data)
  rowwise_df(data, group_vars)
}
