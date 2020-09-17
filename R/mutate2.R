#' @export
mutate2 <- function(.data, ...) {
  UseMethod("mutate2")
}

#' @export
mutate2.data.frame <- function(.data, ...,
                              .keep = c("all", "used", "unused", "none"),
                              .before = NULL, .after = NULL) {
  keep <- arg_match(.keep)

  cols <- mutate2_cols(.data, ...)
  out <- dplyr_col_modify(.data, cols)

  .before <- enquo(.before)
  .after <- enquo(.after)
  if (!quo_is_null(.before) || !quo_is_null(.after)) {
    # Only change the order of new columns
    new <- setdiff(names(cols), names(.data))
    out <- relocate(out, !!new, .before = !!.before, .after = !!.after)
  }

  if (keep == "all") {
    out
  } else if (keep == "unused") {
    unused <- c(names(.data)[!attr(cols, "used")])
    keep <- intersect(names(out), c(unused, names(cols)))
    dplyr_col_select(out, keep)
  } else if (keep == "used") {
    used <- names(.data)[attr(cols, "used")]
    keep <- intersect(names(out), c(used, names(cols)))
    dplyr_col_select(out, keep)
  } else if (keep == "none") {
    keep <- c(
      # ensure group vars present
      setdiff(group_vars(.data), names(cols)),
      # cols might contain NULLs
      intersect(names(cols), names(out))
    )
    dplyr_col_select(out, keep)
  }
}

# Helpers -----------------------------------------------------------------

mutate2_cols <- function(.data, ...) {
  chops <- dplyr_lazy_vec_chop(.data, caller_env(2))
  masks <- dplyr_data_masks(chops, .data)

  dots <- enquos(...)
  auto_names <- names(enquos(..., .named = TRUE))

  ## evaluate all expressions for all groups
  # updated internally before each eval_tidy() call
  context <- env(index_expression = NA_integer_, index_group = NA_integer_)
  lists <- withCallingHandlers(
    .Call(dplyr_eval_tidy_all, dots, chops, masks, caller_env(), auto_names, context),
    error = function(e) {
      index_expression <- context$index_expression
      index_group <- context$index_group

      # TODO: handle when index_group = -1: error in hybrid eval

      local_call_step(dots = dots, .index = index_expression, .fn = "mutate",
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

  vec_unchop(tibbles, group_rows(.data))
}
