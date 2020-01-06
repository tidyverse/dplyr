#' Choose rows by position
#'
#' Choose rows by their ordinal position in the tbl.  Grouped tbls use
#' the ordinal position within the group.
#'
#' Slice does not work with relational databases because they have no
#' intrinsic notion of row order. If you want to perform the equivalent
#' operation, use [filter()] and [row_number()].
#'
#' @family single table verbs
#' @param .data A tbl.
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Integer row values.
#'   Provide either positive values to keep, or negative values to drop.
#'   The values provided must be either all positive or all negative.
#'   Indices beyond the number of rows in the input are silently ignored.
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @export
#' @examples
#' slice(mtcars, 1L)
#' # Similar to tail(mtcars, 1):
#' slice(mtcars, n())
#' slice(mtcars, 5:n())
#' # Rows can be dropped with negative indices:
#' slice(mtcars, -5:-n())
#' # In this case, the result will be equivalent to:
#' slice(mtcars, 1:4)
#'
#' by_cyl <- group_by(mtcars, cyl)
#' slice(by_cyl, 1:2)
#'
#' # Equivalent code using filter that will also work with databases,
#' # but won't be as fast for in-memory data. For many databases, you'll
#' # need to supply an explicit variable to use to compute the row number.
#' filter(mtcars, row_number() == 1L)
#' filter(mtcars, row_number() == n())
#' filter(mtcars, between(row_number(), 5, n()))
slice <- function(.data, ..., .preserve = FALSE) {
  UseMethod("slice")
}


#' @export
slice.tbl_df <- function(.data, ..., .preserve = FALSE) {
  rows <- group_rows(.data)
  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(...)
  if (is_empty(dots)) {
    return(.data)
  }

  rows <- group_rows(.data)
  mask <- DataMask$new(.data, caller_env(), rows)

  quo <- quo(c(!!!dots))

  chunks <- mask$eval_all(quo)

  slice_indices <- new_list(length(rows))
  new_rows <- new_list(length(rows))
  k <- 1L

  for (group in seq_along(rows)) {
    current_rows <- rows[[group]]
    res <- chunks[[group]]

    if (is.logical(res) && all(is.na(res))) {
      res <- integer()
    } else if (is.numeric(res)) {
      res <- vec_cast(res, integer())
    } else if (!is.integer(res)) {
      abort(
        "slice() expressions should return indices (positive or negative integers)",
        "dplyr_slice_incompatible"
      )
    }

    if (length(res) == 0L) {
      # nothing to do
    } else if(all(res >= 0, na.rm = TRUE)) {
      res <- res[!is.na(res) & res <= length(current_rows) & res > 0]
    } else if (all(res <= 0, na.rm = TRUE)) {
      res <- setdiff(seq_along(current_rows), -res)
    } else {
      abort(
        "slice() expressions should return either all positive or all negative",
        "dplyr_slice_ambiguous"
      )
    }

    slice_indices[[group]] <- current_rows[res]
    new_k <- k + length(res)
    new_rows[[group]] <- seq2(k, new_k - 1L)
    k <- new_k
  }
  all_slice_indices <- vec_c(!!!slice_indices, .ptype = integer())

  out <- vec_slice(.data, all_slice_indices)

  if (is_grouped_df(.data)) {
    new_groups <- group_data(.data)
    new_groups$.rows <- new_list_of(new_rows, ptype = integer())
    attr(out, "groups") <- new_groups

    if (!.preserve) {
      out <- regroup(out)
    }
  }

  out
}

#' @export
slice.data.frame <- function(.data, ..., .preserve = FALSE) {
  as.data.frame(slice(tbl_df(.data), ..., .preserve = .preserve))
}
