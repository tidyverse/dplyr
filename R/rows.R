#' Manipulate individual rows
#'
#' @description
#'
#' These functions provide a framework for modifying rows in a table using a
#' second table of data. The two tables are matched `by` a set of key variables
#' whose values typically uniquely identify each row. The functions are inspired
#' by SQL's `INSERT`, `UPDATE`, and `DELETE`, and can optionally modify
#' `in_place` for selected backends.
#'
#' * `rows_insert()` adds new rows (like `INSERT`). By default, key values in
#'   `y` must not exist in `x`.
#' * `rows_append()` works like `rows_insert()` but ignores keys.
#' * `rows_update()` modifies existing rows (like `UPDATE`). Key values in `y`
#'   must be unique, and, by default, key values in `y` must exist in `x`.
#' * `rows_patch()` works like `rows_update()` but only overwrites `NA` values.
#' * `rows_upsert()` inserts or updates depending on whether or not the
#'   key value in `y` already exists in `x`. Key values in `y` must be unique.
#' * `rows_delete()` deletes rows (like `DELETE`). By default, key values in `y`
#'   must exist in `x`.
#'
#' @section Methods:
#' These function are **generic**s, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#'
#' * `rows_insert()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("rows_insert")}.
#' * `rows_append()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("rows_append")}.
#' * `rows_update()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("rows_update")}.
#' * `rows_patch()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("rows_patch")}.
#' * `rows_upsert()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("rows_upsert")}.
#' * `rows_delete()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("rows_delete")}.
#'
#' @inheritParams left_join
#' @param x,y A pair of data frames or data frame extensions (e.g. a tibble).
#'   `y` must have the same columns of `x` or a subset.
#' @param by An unnamed character vector giving the key columns. The key columns
#'   must exist in both `x` and `y`. Keys typically uniquely identify each row,
#'   but this is only enforced for the key values of `y` when `rows_update()`,
#'   `rows_patch()`, or `rows_upsert()` are used.
#'
#'   By default, we use the first column in `y`, since the first column is
#'   a reasonable place to put an identifier variable.
#' @param in_place Should `x` be modified in place? This argument is only
#'   relevant for mutable backends (e.g. databases, data.tables).
#'
#'   When `TRUE`, a modified version of `x` is returned invisibly;
#'   when `FALSE`, a new object representing the resulting changes is returned.
#' @param conflict For `rows_insert()`, how should keys in `y` that conflict
#'   with keys in `x` be handled? A conflict arises if there is a key in `y`
#'   that already exists in `x`.
#'
#'   One of:
#'   - `"error"`, the default, will error if there are any keys in `y` that
#'     conflict with keys in `x`.
#'   - `"ignore"` will ignore rows in `y` with keys that conflict with keys in
#'     `x`.
#' @param unmatched For `rows_update()`, `rows_patch()`, and `rows_delete()`,
#'   how should keys in `y` that are unmatched by the keys in `x` be handled?
#'
#'   One of:
#'   - `"error"`, the default, will error if there are any keys in `y` that
#'     are unmatched by the keys in `x`.
#'   - `"ignore"` will ignore rows in `y` with keys that are unmatched by the
#'     keys in `x`.
#' @returns
#' An object of the same type as `x`. The order of the rows and columns of `x`
#' is preserved as much as possible. The output has the following properties:
#'
#' * `rows_update()` and `rows_patch()` preserve the number of rows;
#'   `rows_insert()`, `rows_append()`, and `rows_upsert()` return all existing
#'   rows and potentially new rows; `rows_delete()` returns a subset of the
#'   rows.
#' * Columns are not added, removed, or relocated, though the data may be
#'   updated.
#' * Groups are taken from `x`.
#' * Data frame attributes are taken from `x`.
#'
#' If `in_place = TRUE`, the result will be returned invisibly.
#' @name rows
#' @examples
#' data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
#' data
#'
#' # Insert
#' rows_insert(data, tibble(a = 4, b = "z"))
#'
#' # By default, if a key in `y` matches a key in `x`, then it can't be inserted
#' # and will throw an error. Alternatively, you can ignore rows in `y`
#' # containing keys that conflict with keys in `x` with `conflict = "ignore"`,
#' # or you can use `rows_append()` to ignore keys entirely.
#' try(rows_insert(data, tibble(a = 3, b = "z")))
#' rows_insert(data, tibble(a = 3, b = "z"), conflict = "ignore")
#' rows_append(data, tibble(a = 3, b = "z"))
#'
#' # Update
#' rows_update(data, tibble(a = 2:3, b = "z"))
#' rows_update(data, tibble(b = "z", a = 2:3), by = "a")
#'
#' # Variants: patch and upsert
#' rows_patch(data, tibble(a = 2:3, b = "z"))
#' rows_upsert(data, tibble(a = 2:4, b = "z"))
#'
#' # Delete and truncate
#' rows_delete(data, tibble(a = 2:3))
#' rows_delete(data, tibble(a = 2:3, b = "b"))
#'
#' # By default, for update, patch, and delete it is an error if a key in `y`
#' # doesn't exist in `x`. You can ignore rows in `y` that have unmatched keys
#' # with `unmatched = "ignore"`.
#' y <- tibble(a = 3:4, b = "z")
#' try(rows_update(data, y, by = "a"))
#' rows_update(data, y, by = "a", unmatched = "ignore")
#' rows_patch(data, y, by = "a", unmatched = "ignore")
#' rows_delete(data, y, by = "a", unmatched = "ignore")
NULL


#' @rdname rows
#' @export
rows_insert <- function(x,
                        y,
                        by = NULL,
                        ...,
                        conflict = c("error", "ignore"),
                        copy = FALSE,
                        in_place = FALSE) {
  UseMethod("rows_insert")
}

#' @export
rows_insert.data.frame <- function(x,
                                   y,
                                   by = NULL,
                                   ...,
                                   conflict = c("error", "ignore"),
                                   copy = FALSE,
                                   in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  by <- rows_check_by(by, y)

  rows_check_x_contains_y(x, y)
  rows_check_contains_by(x, by, "x")
  rows_check_contains_by(y, by, "y")

  y <- rows_cast_y(y, x)

  x_key <- dplyr_col_select(x, by)
  y_key <- dplyr_col_select(y, by)

  keep <- rows_check_y_conflict(x_key, y_key, conflict)

  if (!is.null(keep)) {
    y <- dplyr_row_slice(y, keep)
  }

  rows_bind(x, y)
}

#' @rdname rows
#' @export
rows_append <- function(x,
                        y,
                        ...,
                        copy = FALSE,
                        in_place = FALSE) {
  UseMethod("rows_append")
}

#' @export
rows_append.data.frame <- function(x,
                                   y,
                                   ...,
                                   copy = FALSE,
                                   in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  rows_check_x_contains_y(x, y)
  y <- rows_cast_y(y, x)

  rows_bind(x, y)
}

#' @rdname rows
#' @export
rows_update <- function(x,
                        y,
                        by = NULL,
                        ...,
                        unmatched = c("error", "ignore"),
                        copy = FALSE,
                        in_place = FALSE) {
  UseMethod("rows_update", x)
}

#' @export
rows_update.data.frame <- function(x,
                                   y,
                                   by = NULL,
                                   ...,
                                   unmatched = c("error", "ignore"),
                                   copy = FALSE,
                                   in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  by <- rows_check_by(by, y)

  rows_check_x_contains_y(x, y)
  rows_check_contains_by(x, by, "x")
  rows_check_contains_by(y, by, "y")

  x_key <- dplyr_col_select(x, by)
  y_key <- dplyr_col_select(y, by)

  rows_check_unique(y_key, "y")

  args <- vec_cast_common(x = x_key, y = y_key)
  x_key <- args$x
  y_key <- args$y

  values_names <- setdiff(names(y), names(y_key))

  x_values <- dplyr_col_select(x, values_names)
  y_values <- dplyr_col_select(y, values_names)
  y_values <- rows_cast_y(y_values, x_values)

  keep <- rows_check_y_unmatched(x_key, y_key, unmatched)

  if (!is.null(keep)) {
    y_key <- dplyr_row_slice(y_key, keep)
    y_values <- dplyr_row_slice(y_values, keep)
  }

  loc <- vec_match(x_key, y_key)
  match <- !is.na(loc)

  y_loc <- loc[match]
  x_loc <- which(match)

  y_values <- dplyr_row_slice(y_values, y_loc)

  x_values <- vec_assign(x_values, x_loc, y_values)
  x_values <- dplyr_new_list(x_values)

  x <- dplyr_col_modify(x, x_values)

  x
}

#' @rdname rows
#' @export
rows_patch <- function(x,
                       y,
                       by = NULL,
                       ...,
                       unmatched = c("error", "ignore"),
                       copy = FALSE,
                       in_place = FALSE) {
  UseMethod("rows_patch", x)
}

#' @export
rows_patch.data.frame <- function(x,
                                  y,
                                  by = NULL,
                                  ...,
                                  unmatched = c("error", "ignore"),
                                  copy = FALSE,
                                  in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  by <- rows_check_by(by, y)

  rows_check_x_contains_y(x, y)
  rows_check_contains_by(x, by, "x")
  rows_check_contains_by(y, by, "y")

  x_key <- dplyr_col_select(x, by)
  y_key <- dplyr_col_select(y, by)

  rows_check_unique(y_key, "y")

  args <- vec_cast_common(x = x_key, y = y_key)
  x_key <- args$x
  y_key <- args$y

  values_names <- setdiff(names(y), names(y_key))

  x_values <- dplyr_col_select(x, values_names)
  y_values <- dplyr_col_select(y, values_names)
  y_values <- rows_cast_y(y_values, x_values)

  keep <- rows_check_y_unmatched(x_key, y_key, unmatched)

  if (!is.null(keep)) {
    y_key <- dplyr_row_slice(y_key, keep)
    y_values <- dplyr_row_slice(y_values, keep)
  }

  loc <- vec_match(x_key, y_key)
  match <- !is.na(loc)

  y_loc <- loc[match]
  x_loc <- which(match)

  x_slice <- dplyr_row_slice(x_values, x_loc)
  x_slice <- dplyr_new_list(x_slice)

  y_slice <- dplyr_row_slice(y_values, y_loc)
  y_slice <- dplyr_new_list(y_slice)

  x_patched <- map2(x_slice, y_slice, coalesce)
  x_patched <- new_data_frame(x_patched, n = length(x_loc))

  x_values <- vec_assign(x_values, x_loc, x_patched)
  x_values <- dplyr_new_list(x_values)

  x <- dplyr_col_modify(x, x_values)

  x
}

#' @rdname rows
#' @export
rows_upsert <- function(x,
                        y,
                        by = NULL,
                        ...,
                        copy = FALSE,
                        in_place = FALSE) {
  UseMethod("rows_upsert", x)
}

#' @export
rows_upsert.data.frame <- function(x,
                                   y,
                                   by = NULL,
                                   ...,
                                   copy = FALSE,
                                   in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  by <- rows_check_by(by, y)

  rows_check_x_contains_y(x, y)
  rows_check_contains_by(x, by, "x")
  rows_check_contains_by(y, by, "y")

  x_key <- dplyr_col_select(x, by)
  y_key <- dplyr_col_select(y, by)

  rows_check_unique(y_key, "y")

  args <- vec_cast_common(x = x_key, y = y_key)
  x_key <- args$x
  y_key <- args$y

  values_names <- setdiff(names(y), names(y_key))

  x_values <- dplyr_col_select(x, values_names)
  y_values <- dplyr_col_select(y, values_names)
  y_values <- rows_cast_y(y_values, x_values)

  loc <- vec_match(x_key, y_key)
  match <- !is.na(loc)

  y_loc <- loc[match]
  x_loc <- which(match)

  # Update
  y_values <- dplyr_row_slice(y_values, y_loc)
  x_values <- vec_assign(x_values, x_loc, y_values)
  x_values <- dplyr_new_list(x_values)

  x <- dplyr_col_modify(x, x_values)

  # Insert
  y_size <- vec_size(y_key)
  y_extra <- vec_as_location_invert(y_loc, y_size)

  y <- dplyr_row_slice(y, y_extra)
  y <- rows_cast_y(y, x)

  x <- rows_bind(x, y)

  x
}

#' @rdname rows
#' @export
rows_delete <- function(x,
                        y,
                        by = NULL,
                        ...,
                        unmatched = c("error", "ignore"),
                        copy = FALSE,
                        in_place = FALSE) {
  UseMethod("rows_delete", x)
}

#' @export
rows_delete.data.frame <- function(x,
                                   y,
                                   by = NULL,
                                   ...,
                                   unmatched = c("error", "ignore"),
                                   copy = FALSE,
                                   in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  by <- rows_check_by(by, y)

  rows_check_contains_by(x, by, "x")
  rows_check_contains_by(y, by, "y")

  x_key <- dplyr_col_select(x, by)
  y_key <- dplyr_col_select(y, by)

  args <- vec_cast_common(x = x_key, y = y_key)
  x_key <- args$x
  y_key <- args$y

  keep <- rows_check_y_unmatched(x_key, y_key, unmatched)

  if (!is.null(keep)) {
    y_key <- dplyr_row_slice(y_key, keep)
  }

  extra <- setdiff(names(y), names(y_key))
  if (!is_empty(extra)) {
    message <- glue("Ignoring extra `y` columns: ", commas(tick_if_needed(extra)))
    inform(message, class = c("dplyr_message_delete_extra_cols", "dplyr_message"))
  }

  loc <- vec_match(x_key, y_key)
  unmatched <- is.na(loc)

  x_loc <- which(unmatched)

  dplyr_row_slice(x, x_loc)
}

# helpers -----------------------------------------------------------------

rows_check_by <- function(by, y, ..., error_call = caller_env()) {
  check_dots_empty()

  if (is.null(by)) {
    if (ncol(y) == 0L) {
      abort("`y` must have at least one column.", call = error_call)
    }

    by <- names(y)[[1]]

    inform(
      message = glue("Matching, by = \"{by}\""),
      class = c("dplyr_message_matching_by", "dplyr_message")
    )
  }

  if (!is.character(by)) {
    abort("`by` must be a character vector.", call = error_call)
  }
  if (is_empty(by)) {
    abort("`by` must specify at least 1 column.", call = error_call)
  }
  if (!all(names2(by) == "")) {
    abort("`by` must be unnamed.", call = error_call)
  }

  by
}

rows_check_x_contains_y <- function(x, y, ..., error_call = caller_env()) {
  check_dots_empty()

  bad <- setdiff(names(y), names(x))

  if (!is_empty(bad)) {
    bad <- err_vars(bad)

    message <- c(
      "All columns in `y` must exist in `x`.",
      i = glue("The following columns only exist in `y`: {bad}.")
    )

    abort(message, call = error_call)
  }

  invisible()
}

rows_cast_y <- function(y, x, ..., call = caller_env()) {
  vec_cast(x = y, to = x, x_arg = "y", to_arg = "x", call = call)
}

rows_check_contains_by <- function(x, by, arg, ..., error_call = caller_env()) {
  check_dots_empty()

  missing <- setdiff(by, names(x))

  if (is_empty(missing)) {
    return(invisible())
  }

  missing <- err_vars(missing)

  message <- c(
    "All columns specified through `by` must exist in `x` and `y`.",
    i = glue("The following columns are missing from `{arg}`: {missing}.")
  )

  abort(message, call = error_call)
}

rows_check_unique <- function(x, arg, ..., error_call = caller_env()) {
  check_dots_empty()

  if (!vec_duplicate_any(x)) {
    return(invisible())
  }

  duplicated <- vec_duplicate_detect(x)
  duplicated <- which(duplicated)
  duplicated <- err_locs(duplicated)

  message <- c(
    glue("`{arg}` key values must be unique."),
    i = glue("The following rows contain duplicate key values: {duplicated}.")
  )

  abort(message, call = error_call)
}

rows_check_y_conflict <- function(x_key,
                                  y_key,
                                  conflict,
                                  ...,
                                  error_call = caller_env()) {
  check_dots_empty()

  conflict <- rows_check_conflict(conflict, error_call = error_call)

  keep <- NULL
  rows_matched <- vec_in(y_key, x_key)

  if (any(rows_matched)) {
    if (conflict == "error") {
      rows_matched <- which(rows_matched)
      rows_matched <- err_locs(rows_matched)

      message <- c(
        "`y` can't contain keys that already exist in `x`.",
        i = glue("The following rows in `y` have keys that already exist in `x`: {rows_matched}."),
        i = "Use `conflict = \"ignore\"` if you want to ignore these `y` rows."
      )

      abort(message, call = error_call)
    } else if (conflict == "ignore") {
      keep <- which(!rows_matched)
    } else {
      abort("Unknown `conflict` value.", .internal = TRUE)
    }
  }

  keep
}

rows_check_y_unmatched <- function(x_key,
                                   y_key,
                                   unmatched,
                                   ...,
                                   error_call = caller_env()) {
  check_dots_empty()

  unmatched <- rows_check_unmatched(unmatched, error_call = error_call)

  keep <- NULL
  rows_unmatched <- !vec_in(y_key, x_key)

  if (any(rows_unmatched)) {
    if (unmatched == "error") {
      rows_unmatched <- which(rows_unmatched)
      rows_unmatched <- err_locs(rows_unmatched)

      message <- c(
        "`y` must contain keys that already exist in `x`.",
        i = glue("The following rows in `y` have keys that don't exist in `x`: {rows_unmatched}."),
        i = "Use `unmatched = \"ignore\"` if you want to ignore these `y` rows."
      )

      abort(message, call = error_call)
    } else if (unmatched == "ignore") {
      keep <- which(!rows_unmatched)
    } else {
      abort("Unknown `unmatched` value.", .internal = TRUE)
    }
  }

  keep
}

rows_check_conflict <- function(conflict, ..., error_call = caller_env()) {
  check_dots_empty()

  arg_match(
    arg = conflict,
    values = c("error", "ignore"),
    error_arg = "conflict",
    error_call = error_call
  )
}

rows_check_unmatched <- function(unmatched, ..., error_call = caller_env()) {
  check_dots_empty()

  arg_match(
    arg = unmatched,
    values = c("error", "ignore"),
    error_arg = "unmatched",
    error_call = error_call
  )
}

rows_df_in_place <- function(in_place, error_call = caller_env()) {
  if (is_true(in_place)) {
    msg <- "Data frames only support `in_place = FALSE`."
    abort(msg, call = error_call)
  }
}

rows_bind <- function(x, y) {
  dplyr_reconstruct(vctrs::vec_rbind(x, y), x)
}

vec_as_location_invert <- function(i, n) {
  if (is_empty(i)) {
    seq_len(n)
  } else {
    vec_as_location(-i, n)
  }
}
