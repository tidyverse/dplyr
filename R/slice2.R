#' @export
slice2 <- function(.data, ..., .preserve = FALSE) {
  UseMethod("slice2")
}

#' @export
slice2.data.frame <- function(.data, ..., .preserve = FALSE) {
  loc <- slice2_rows(.data, ...)
  dplyr_row_slice(.data, loc, preserve = .preserve)
}

slice2_rows <- function(.data, ...) {
  dots <- enquos(...)
  if (is_empty(dots)) {
    return(TRUE)
  }

  chops <- dplyr_lazy_vec_chop(.data, caller_env(2))
  masks <- dplyr_data_masks(chops, .data)

  dots <- list(x = quo(c(!!!dots)))

  context <- env(index_expression = NA_integer_, index_group = NA_integer_)
  lists <- withCallingHandlers(
    .Call(dplyr_eval_tidy_all, dots, chops, masks, caller_env(), "x", context),
    error = function(e) {
      index_expression <- context$index_expression
      index_group <- context$index_group

      # TODO: handle when index_group = -1: error in hybrid eval

      local_call_step(dots = dots, .index = index_expression, .fn = "slice",
                      .dot_data = inherits(e, "rlang_error_data_pronoun_not_found"))

      bullets <- c(
        cnd_bullet_header(),
        x = conditionMessage(e),
        i = cnd_bullet_input_info()
      )
      if (is_grouped_df(.data)) {
        keys <- group_keys(.data)[index_group, ]
        bullets <- c(bullets, i = glue("The error occurred in group {index_group}: {group_labels_details(keys)}."))
      }
      abort(bullets)
    }
  )

  chunks <- map(lists, `[[`, 1)
  rows <- group_rows(.data)

  slice_indices <- new_list(length(rows))

  for (group in seq_along(rows)) {
    current_rows <- rows[[group]]
    res <- chunks[[group]]

    if (is.logical(res) && all(is.na(res))) {
      res <- integer()
    } else if (is.numeric(res)) {
      res <- vec_cast(res, integer())
    } else if (!is.integer(res)) {
      abort("`slice()` expressions should return indices (positive or negative integers).")
    }

    if (length(res) == 0L) {
      # nothing to do
    } else if (all(res >= 0, na.rm = TRUE)) {
      res <- res[!is.na(res) & res <= length(current_rows) & res > 0]
    } else if (all(res <= 0, na.rm = TRUE)) {
      res <- setdiff(seq_along(current_rows), -res)
    } else {
      abort("`slice()` expressions should return either all positive or all negative.")
    }

    slice_indices[[group]] <- current_rows[res]
  }

  vec_c(!!!slice_indices, .ptype = integer())
}
