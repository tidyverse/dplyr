#' @export
filter2 <- function(.data, ..., .preserve = FALSE) {
  UseMethod("filter2")
}

#' @export
filter2.data.frame <- function(.data, ..., .preserve = FALSE) {
  loc <- filter2_rows(.data, ...)
  dplyr_row_slice(.data, loc, preserve = .preserve)
}

filter2_rows <- function(.data, ...) {
  dots <- check_filter(enquos(...))

  # once we have checked, we can "pretend" to have names,
  # as they are expected and used internally in dplyr_eval_tidy_all
  names(dots) <- paste0("x", seq_along(dots))

  chops <- dplyr_lazy_vec_chop(.data, caller_env(2))
  masks <- dplyr_data_masks(chops, .data)

  context <- env(index_expression = NA_integer_, index_group = NA_integer_)
  lists <- withCallingHandlers(
    .Call(dplyr_eval_tidy_all, dots, chops, masks, caller_env(), names(dots), context),
    error = function(e) {
      index_expression <- context$index_expression
      index_group <- context$index_group

      # TODO: handle when index_group = -1: error in hybrid eval

      local_call_step(dots = dots, .index = index_expression, .fn = "filter",
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

  tibbles <- map(lists, function(.x) {
    new_data_frame(df_list(!!!.x))
  })
  keep <- map(tibbles, function(.x) {
    rowSums(.x) == ncol(.x)
  })
  keep <- vec_unchop(keep, group_rows(.data))
  keep
}
