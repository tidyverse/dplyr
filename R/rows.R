#' Manipulate individual rows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' These functions provide a framework for modifying rows in a table using a
#' second table of data. The two tables are matched `by` a set of key variables
#' whose values typically uniquely identify each row. The functions are inspired
#' by SQL's `INSERT`, `UPDATE`, and `DELETE`, and can optionally modify
#' `in_place` for selected backends.
#'
#' * `rows_insert()` adds new rows (like `INSERT`). Key values in `y` must
#'    not occur in `x`.
#' * `rows_update()` modifies existing rows (like `UPDATE`). Key values in
#'   `y` must occur in `x`, and key values in `y` must be unique.
#' * `rows_patch()` works like `rows_update()` but only overwrites `NA` values.
#' * `rows_upsert()` inserts or updates depending on whether or not the
#'   key value in `y` already exists in `x`. Key values in `y` must be unique.
#' * `rows_delete()` deletes rows (like `DELETE`). Key values in `y` must
#'   exist in `x`.
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
#' @returns
#' An object of the same type as `x`. The order of the rows and columns of `x`
#' is preserved as much as possible. The output has the following properties:
#'
#' * `rows_update()` preserves rows as is; `rows_insert()` and `rows_upsert()`
#'   return all existing rows and potentially new rows; `rows_delete()` returns
#'   a subset of the rows.
#' * Columns are not added, removed, or relocated, though the data may be updated.
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
#' try(rows_insert(data, tibble(a = 3, b = "z")))
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
#' try(rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b")))
NULL


#' @rdname rows
#' @export
rows_insert <- function(x,
                        y,
                        by = NULL,
                        ...,
                        copy = FALSE,
                        in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_insert()")
  UseMethod("rows_insert")
}

#' @export
rows_insert.data.frame <- function(x,
                                   y,
                                   by = NULL,
                                   ...,
                                   copy = FALSE,
                                   in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  x_key <- rows_select_key(x, by, "x")
  y_key <- rows_select_key(y, by, "y")

  rows_check_y_matched(x_key, y_key)

  rows_bind(x, y)
}

#' @rdname rows
#' @export
rows_update <- function(x,
                        y,
                        by = NULL,
                        ...,
                        copy = FALSE,
                        in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_update()")
  UseMethod("rows_update", x)
}

#' @export
rows_update.data.frame <- function(x,
                                   y,
                                   by = NULL,
                                   ...,
                                   copy = FALSE,
                                   in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  x_key <- rows_select_key(x, by, "x")
  y_key <- rows_select_key(y, by, "y", unique = TRUE)

  rows_check_y_unmatched(x_key, y_key)

  loc <- vec_match(x_key, y_key)
  match <- !is.na(loc)

  y_loc <- loc[match]
  x_loc <- which(match)

  values_cols <- setdiff(names(y), names(y_key))

  x[x_loc, values_cols] <- y[y_loc, values_cols]

  x
}

#' @rdname rows
#' @export
rows_patch <- function(x,
                       y,
                       by = NULL,
                       ...,
                       copy = FALSE,
                       in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_patch()")
  UseMethod("rows_patch", x)
}

#' @export
rows_patch.data.frame <- function(x,
                                  y,
                                  by = NULL,
                                  ...,
                                  copy = FALSE,
                                  in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  x_key <- rows_select_key(x, by, "x")
  y_key <- rows_select_key(y, by, "y", unique = TRUE)

  rows_check_y_unmatched(x_key, y_key)

  loc <- vec_match(x_key, y_key)
  match <- !is.na(loc)

  y_loc <- loc[match]
  x_loc <- which(match)

  values_cols <- setdiff(names(y), names(y_key))

  x_values <- x[x_loc, values_cols]
  y_values <- y[y_loc, values_cols]

  x_patched <- map2(x_values, y_values, coalesce)

  x[x_loc, values_cols] <- x_patched

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
  lifecycle::signal_stage("experimental", "rows_upsert()")
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

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  x_key <- rows_select_key(x, by, "x")
  y_key <- rows_select_key(y, by, "y", unique = TRUE)

  loc <- vec_match(x_key, y_key)
  match <- !is.na(loc)

  y_loc <- loc[match]
  x_loc <- which(match)

  y_size <- vec_size(y_key)
  y_extra <- vec_as_location_invert(y_loc, y_size)
  y_extra <- dplyr_row_slice(y, y_extra)

  values_cols <- setdiff(names(y), names(y_key))

  x[x_loc, values_cols] <- y[y_loc, values_cols]
  x <- rows_bind(x, y_extra)

  x
}

#' @rdname rows
#' @export
rows_delete <- function(x,
                        y,
                        by = NULL,
                        ...,
                        copy = FALSE,
                        in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_delete()")
  UseMethod("rows_delete", x)
}

#' @export
rows_delete.data.frame <- function(x,
                                   y,
                                   by = NULL,
                                   ...,
                                   copy = FALSE,
                                   in_place = FALSE) {
  check_dots_empty()
  rows_df_in_place(in_place)

  y <- auto_copy(x, y, copy = copy)

  by <- rows_check_by(by, y)

  x_key <- rows_select_key(x, by, "x")
  y_key <- rows_select_key(y, by, "y")

  rows_check_y_unmatched(x_key, y_key)

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

rows_check_containment <- function(x, y, ..., error_call = caller_env()) {
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

rows_select_key <- function(x,
                            by,
                            arg,
                            ...,
                            unique = FALSE,
                            error_call = caller_env()) {
  check_dots_empty()

  missing <- setdiff(by, names(x))

  if (!is_empty(missing)) {
    missing <- err_vars(missing)

    message <- c(
      "All columns specified through `by` must exist in `x` and `y`.",
      i = glue("The following columns are missing from `{arg}`: {missing}.")
    )

    abort(message, call = error_call)
  }

  out <- x[by]

  if (unique && vec_duplicate_any(out)) {
    duplicated <- vec_duplicate_detect(out)
    duplicated <- which(duplicated)
    duplicated <- err_locs(duplicated)

    message <- c(
      glue("`{arg}` key values must be unique."),
      i = glue("The following rows contain duplicate key values: {duplicated}.")
    )

    abort(message, call = error_call)
  }

  out
}

rows_check_y_matched <- function(x_key,
                                 y_key,
                                 ...,
                                 error_call = caller_env()) {
  check_dots_empty()

  matched <- vec_in(y_key, x_key)

  if (any(matched)) {
    matched <- which(matched)
    matched <- err_locs(matched)

    message <- c(
      "`y` must contain keys that don't exist in `x`.",
      i = glue("The following rows in `y` have keys that already exist in `x`: {matched}.")
    )

    abort(message, call = error_call)
  }
}

rows_check_y_unmatched <- function(x_key,
                                   y_key,
                                   ...,
                                   error_call = caller_env()) {
  check_dots_empty()

  unmatched <- !vec_in(y_key, x_key)

  if (any(unmatched)) {
    unmatched <- which(unmatched)
    unmatched <- err_locs(unmatched)

    message <- c(
      "`y` must contain keys that already exist in `x`.",
      i = glue("The following rows in `y` have keys that don't exist in `x`: {unmatched}.")
    )

    abort(message, call = error_call)
  }
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
