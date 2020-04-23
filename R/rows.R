#' Manipulate individual rows
#'
#' @description
#' \lifecycle{experimental}
#'
#' These methods provide a framework for manipulating individual rows
#' in existing tables, modeled after the SQL operations
#' `INSERT`, `UPDATE` and `DELETE`.
#' All operations expect existing and new data to be compatible.
#'
#' @inheritParams left_join
#' @param x Target table object.
#' @param y Source table object. All columns in `y` must exist in `x`.
#' @param by Key columns as unnamed character vector.
#'   All columns in `by` must exist in `y` (and, by extension, in `x`).
#'   The default is the first column of `y`.
#' @param inplace
#'   This argument is only relevant for mutable backends,
#'   e.g. databases or \pkg{dtplyr}.
#'   For data frames, these operations always return a modified copy
#'   of the data.
#'   An informative message is given if set to `TRUE`.
#'
#'   For mutable backends, set to `FALSE` for running the operation
#'   without updating the data in place.
#'   In this mode, a modified version of `x` is returned, as for data frames.
#'   This allows verifying the results of an operation before actually
#'   applying it.
#'   Set to `TRUE` to perform the update on the remote table.
#'   The default is `FALSE` with an informative message.
#'
#' @return A tbl object of the same structure as `x`.
#'   On mutable backends, same as `x` if `inplace = TRUE`.
#'
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


#' rows_insert
#'
#' `rows_insert()` adds new rows.
#' This operation corresponds to `INSERT` in SQL.
#' If `by` is non-empty, no two rows with the same values in the key columns
#' are permitted.
#' @rdname rows
#' @export
rows_insert <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  check_rows_args(x, y, by)
  UseMethod("rows_insert")
}

#' @export
rows_insert.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_inplace(inplace)

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

#' rows_update
#'
#' `rows_update()` updates existing rows.
#' This operation corresponds to `UPDATE` in SQL.
#' `by` is mandatory and defaults to the first column in `y`.
#' No two rows with the same values may exist in the new data.
#' Each row in the new data must have exactly one corresponding row
#' in the existing data.
#' @rdname rows
#' @export
rows_update <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  check_rows_args(x, y, by)
  UseMethod("rows_update", x)
}

#' @export
rows_update.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_inplace(inplace)

  idx <- vctrs::vec_match(y[y_key], x[x_key])
  # FIXME: Check key in x? https://github.com/r-lib/vctrs/issues/1032
  x[idx, names(y)] <- y
  x
}

#' rows_patch
#'
#' `rows_patch()` replaces missing values in existing rows.
#' This operation corresponds to `UPDATE` using `COALESCE` expressions in SQL.
#' It is similar to `rows_update()`, leaves non-missing values untouched.
#' `by` is mandatory and defaults to the first column in `y`.
#' No two rows with the same values may exist in the new data.
#' Each row in the new data must have exactly one corresponding row
#' in the existing data.
#' @rdname rows
#' @export
rows_patch <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  check_rows_args(x, y, by)
  UseMethod("rows_patch", x)
}

#' @export
rows_patch.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_inplace(inplace)

  idx <- vctrs::vec_match(y[y_key], x[x_key])
  # FIXME: Check key in x? https://github.com/r-lib/vctrs/issues/1032

  # FIXME: Do we need vec_coalesce()
  new_data <- map2(x[idx, names(y)], y, coalesce)

  x[idx, names(y)] <- new_data
  x
}

#' rows_upsert
#'
#' `rows_upsert()` updates matching rows and adds new rows for mismatches.
#' This operation corresponds to `INSERT ON DUPLICATE KEY UPDATE` or
#' `INSERT ON CONFLICT` in some SQL variants.
#' `by` is mandatory and defaults to the first column in `y`.
#' No two rows with the same values may exist in the new data.
#' Each row in the new data must have exactly one corresponding row
#' in the existing data.
#' @rdname rows
#' @export
rows_upsert <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  check_rows_args(x, y, by)
  UseMethod("rows_upsert", x)
}

#' @export
rows_upsert.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_inplace(inplace)

  idx <- vctrs::vec_match(y[y_key], x[x_key])
  # FIXME: Check key in x?

  new <- is.na(idx)
  idx_existing <- idx[!new]
  idx_new <- idx[new]

  x[idx_existing, names(y)] <- y[!new, ]
  vctrs::vec_rbind(x, y[new, ])
}

#' rows_delete
#'
#' `rows_delete()` deletes existing rows.
#' This operation corresponds to `DELETE` in SQL.
#' `by` is mandatory and defaults to the first column in `y`.
#' @rdname rows
#' @export
rows_delete <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  check_rows_args(x, y, by)
  UseMethod("rows_delete", x)
}

#' @export
rows_delete.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, inplace = NULL) {
  y <- auto_copy(x, y, copy = copy)
  y_key <- df_key(y, by)
  x_key <- df_key(x, names(y_key))
  df_inplace(inplace)

  idx <- vctrs::vec_match(y[y_key], x[x_key])
  # FIXME: Check key in x? https://github.com/r-lib/vctrs/issues/1032
  x[-idx[!is.na(idx)], ]
}

#' rows_truncate
#'
#' `rows_truncate()` removes all rows.
#' This operation corresponds to `TRUNCATE` in SQL.
#' `...` is ignored.
#' @rdname rows
#' @export
rows_truncate <- function(x, ..., copy = FALSE, inplace = NULL) {
  UseMethod("rows_truncate", x)
}

#' @export
rows_truncate.data.frame <- function(x, ..., copy = FALSE, inplace = NULL) {
  df_inplace(inplace)

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

df_inplace <- function(inplace) {
  if (is_true(inplace)) {
    inform("`rows_...()` ignore `inplace` argument for data frames.")
  }
}
