#' Manipulate individual rows
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' These functions provide a framework for modifying rows in table using
#' a second table of data. The two tables are matched `by` a set of key
#' variables whose values must uniquely identify each row. The functions are
#' inspired by SQL's `INSERT`, `UPDATE` and `DELETE`, and can optionally
#' modified `in_place` for selected backends.
#'
#' * `rows_insert()` adds new rows (like `INSERT`); key values in `y` must
#'    not occur in `x`.
#' * `rows_update()` modifies existing rows (like `UPDATE`); key values in
#'   `y` must occur in `x`.
#' * `rows_patch()` works like `rows_update()` but only overwrites `NA` values.
#' * `rows_upsert()` inserts or updates depending on whether or not the the
#'   key value in `x` exists in `y`.
#' * `rows_delete()` deletes rows (like `DELETE`); key values in `y` must
#'   exist in `x`.
#'
#' @inheritParams left_join
#' @param x,y A pair of data frames or data frame extensions (e.g. a tibble).
#' @param by An unnamed character vector giving the key columns. The key
#'   values must uniquely identify each row (i.e. each combination of key
#'   values occurs at most once), and the key columns must exist in both `x`
#'   and `y`.
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
#' * Columns are not modified.
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
#' rows_delete(data, tibble(a = 2:4))
#' rows_delete(data, tibble(a = 2:4, b = "b"))
#' rows_delete(data, tibble(a = 2:4, b = "b"), by = c("a", "b"))
#' rows_truncate(data)
NULL


#' @rdname rows
#' @export
rows_insert <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  ellipsis::check_dots_used()

  check_rows_args(x, y, by)
  UseMethod("rows_insert")
}

#' @export
rows_insert.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_in_place(in_place)

  if (has_length(y_key)) {
    idx <- vctrs::vec_match(y[y_key], x[x_key])
    bad <- which(!is.na(idx))
    if (has_length(bad)) {
      abort(class = "dplyr_rows_insert_duplicate",
        "Attempting to insert duplicate rows.",
        location = bad
      )
    }
  }

  vctrs::vec_rbind(x, y)
}

#' @rdname rows
#' @export
rows_update <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  ellipsis::check_dots_used()

  check_rows_args(x, y, by)
  UseMethod("rows_update", x)
}

#' @export
rows_update.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_in_place(in_place)

  idx <- vctrs::vec_match(y[y_key], x[x_key])
  # FIXME: Check key in x? https://github.com/r-lib/vctrs/issues/1032
  x[idx, names(y)] <- y
  x
}

#' @rdname rows
#' @export
rows_patch <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  ellipsis::check_dots_used()

  check_rows_args(x, y, by)
  UseMethod("rows_patch", x)
}

#' @export
rows_patch.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_in_place(in_place)

  idx <- vctrs::vec_match(y[y_key], x[x_key])
  # FIXME: Check key in x? https://github.com/r-lib/vctrs/issues/1032

  # FIXME: Do we need vec_coalesce()
  new_data <- map2(x[idx, names(y)], y, coalesce)

  x[idx, names(y)] <- new_data
  x
}

#' @rdname rows
#' @export
rows_upsert <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  ellipsis::check_dots_used()

  check_rows_args(x, y, by)
  UseMethod("rows_upsert", x)
}

#' @export
rows_upsert.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_in_place(in_place)

  idx <- vctrs::vec_match(y[y_key], x[x_key])
  # FIXME: Check key in x?

  new <- is.na(idx)
  idx_existing <- idx[!new]
  idx_new <- idx[new]

  x[idx_existing, names(y)] <- y[!new, ]
  vctrs::vec_rbind(x, y[new, ])
}

#' @rdname rows
#' @export
rows_delete <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  ellipsis::check_dots_used()

  check_rows_args(x, y, by)
  UseMethod("rows_delete", x)
}

#' @export
rows_delete.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_in_place(in_place)

  idx <- vctrs::vec_match(y[y_key], x[x_key])
  # FIXME: Check key in x? https://github.com/r-lib/vctrs/issues/1032
  x[-idx[!is.na(idx)], ]
}

#' rows_truncate
#'
#' `rows_truncate()` removes all rows.
#' This operation corresponds to `TRUNCATE` in SQL.
#' `...` is ignored.
#'
#' @inheritParams rows_insert
#' @export
rows_truncate <- function(x, ..., copy = FALSE, in_place = FALSE) {
  ellipsis::check_dots_used()
  UseMethod("rows_truncate", x)
}

#' @export
rows_truncate.data.frame <- function(x, ..., copy = FALSE, in_place = FALSE) {
  df_in_place(in_place)

  x[integer(), ]
}



check_rows_args <- function(x, y, by) {
  check_col_subset(y, x)
  check_by(by, y)
}

check_col_subset <- function(y, x) {
  bad <- setdiff(colnames(y), colnames(x))
  if (has_length(bad)) {
    abort(class = "dplyr_rows_extra_column",
      "All columns in `y` must exist in `x`.",
      name = bad
    )
  }
}

check_by <- function(by, y) {
  if (!is.null(by) && !is.character(by)) {
    abort("`by` must be `NULL` or a character vector.")
  }
  if (!is.null(names(by)) || any(names(by) != "")) {
    abort("`by` must not have names.")
  }
  bad <- setdiff(by, colnames(y))
  if (has_length(bad)) {
    abort(class = "dplyr_rows_extra_column",
      "All `by` columns must exist in `y`.",
      name = bad
    )
  }
}

df_key <- function(y, by) {
  if (is.null(by)) {
    set_names(1L, names(y)[[1]])
  } else {
    idx <- match(by, names(y))
    set_names(idx, by)
  }
}

df_in_place <- function(in_place) {
  if (is_true(in_place)) {
    abort("Data frames only support `in_place = FALSE`")
  }
}
