#' Extending dplyr with new data frame subclasses
#'
#' @description
#' `r lifecycle::badge("experimental")`
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
#'   The one exception to this is if your subclass extends a data.frame
#'   directly rather than extending a tibble. The `[.data.frame` method does not
#'   preserve attributes, so you'll need to write a `[` method for your subclass
#'   that preserves attributes important for your class.
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
#' * `arrange()`, `filter()`, `slice()` (and the rest of the `slice_*()`
#'   family), `semi_join()`, and `anti_join()` work by generating a vector of
#'   row indices, and then subsetting with `dplyr_row_slice()`.
#'
#' * `mutate()` generates a list of new column value (using `NULL` to indicate
#'   when columns should be deleted), then passes that to `dplyr_col_modify()`.
#'   It also uses 1d `[` to implement `.keep`, and will call `relocate()` if
#'   either `.before` or `.after` are supplied.
#'
#' * `summarise()` and `reframe()` work similarly to `mutate()` but the data
#'   modified by `dplyr_col_modify()` comes from `group_data()` or is built
#'   from `.by`.
#'
#' * `select()` uses 1d `[` to select columns, then `names<-` to rename them.
#'   `rename()` just uses `names<-`. `relocate()` just uses 1d `[`.
#'
#' * `inner_join()`, `left_join()`, `right_join()`, and `full_join()`
#'   coerce `x` to a tibble, modify the rows, then use `dplyr_reconstruct()`
#'   to convert back to the same type as `x`.
#'
#' * `nest_join()` converts both `x` and `y` to tibbles, modifies the rows,
#'   and uses `dplyr_col_modify()` to handle modified key variables and the
#'   list-column that `y` becomes. It also uses `dplyr_reconstruct()` to convert
#'   the outer result back to the type of `x`, and to convert the nested tibbles
#'   back to the type of `y`.
#'
#' * `distinct()` does a `mutate()` if any expressions are present, then
#'   uses 1d `[` to select variables to keep, then `dplyr_row_slice()` to
#'   select distinct rows.
#'
#' Note that `group_by()` and `ungroup()` don't use any of these generics and
#' you'll need to provide methods for them directly, or rely on `.by` for
#' per-operation grouping.
#'
#' @keywords internal
#' @param data A tibble. We use tibbles because they avoid some inconsistent
#'    subset-assignment use cases.
#' @name dplyr_extending
NULL

#' @export
#' @rdname dplyr_extending
#' @param i A numeric or logical vector that indexes the rows of `data`.
dplyr_row_slice <- function(data, i, ...) {
  if (!is.numeric(i) && !is.logical(i)) {
    abort("`i` must be a numeric or logical vector.")
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
#' @param cols A named list used to modify columns. A `NULL` value should remove
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

  # Transform to list to avoid stripping inner names with `[[<-`
  out <- as.list(vec_data(data))

  nms <- as_utf8_character(names2(cols))
  names(out) <- as_utf8_character(names2(out))

  for (i in seq_along(cols)) {
    nm <- nms[[i]]
    out[[nm]] <- cols[[i]]
  }

  # Transform back to data frame before reconstruction
  row_names <- .row_names_info(data, type = 0L)
  out <- new_data_frame(out, n = nrow(data), row.names = row_names)

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

#' @param template Template data frame to use for restoring attributes.
#' @export
#' @rdname dplyr_extending
dplyr_reconstruct <- function(data, template) {
  # Strip attributes before dispatch to make it easier to implement
  # methods and prevent unexpected leaking of irrelevant attributes.
  # This also enforces that `data` is a well-formed data frame.
  data <- dplyr_new_data_frame(data)
  return(dplyr_reconstruct_dispatch(data, template))
  UseMethod("dplyr_reconstruct", template)
}
dplyr_reconstruct_dispatch <- function(data, template) {
  UseMethod("dplyr_reconstruct", template)
}

#' @export
dplyr_reconstruct.data.frame <- function(data, template) {
  .Call(ffi_dplyr_reconstruct, data, template)
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

dplyr_col_select <- function(.data, loc, error_call = caller_env()) {
  loc <- vec_as_location(loc, n = ncol(.data), names = names(.data))

  out <- .data[loc]
  if (!inherits(out, "data.frame")) {
    classes_data <- glue_collapse(class(.data), sep = "/")
    classes_out  <- glue_collapse(class(out), sep = "/")
    bullets <- c(
      "Can't reconstruct data frame.",
      x = glue("The `[` method for class <{classes_data}> must return a data frame."),
      i = glue("It returned a <{classes_out}>.")
    )
    abort(bullets, call = error_call)
  }
  if (length(out) != length(loc)) {
    classes_data <- glue_collapse(class(.data), sep = "/")
    classes_out  <- glue_collapse(class(out), sep = "/")
    s <- function(x) if (length(x) == 1) "" else "s"
    bullets <- c(
      "Can't reconstruct data frame.",
      x = glue("The `[` method for class <{classes_data}> must return a data frame with {length(loc)} column{s(loc)}."),
      i = glue("It returned a <{classes_out}> of {length(out)} column{s(out)}.")
    )
    abort(bullets, call = error_call)
  }

  # Patch base data frames and data.table (#6171) to restore extra attributes that `[.data.frame` drops.
  # We require `[` methods to keep extra attributes for all data frame subclasses.
  if (identical(class(.data), "data.frame") || identical(class(.data), c("data.table", "data.frame"))) {
    out <- dplyr_reconstruct(out, .data)
  }

  out
}
