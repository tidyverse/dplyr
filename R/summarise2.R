summarise2_cols <- function(.data, ...) {
  chops <- dplyr_lazy_vec_chop(.data)
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

      local_call_step(dots = dots, .index = index_expression, .fn = "summarise",
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

  ## combine results
  # TODO: promote errors to dplyr style errors.
  # TODO: handle when columns repeat, e.g. summarise(x = 1, x = x + 1)
  tibbles <- map(lists, function(.x) {
    new_data_frame(df_list(!!!.x))
  })
  sizes <- list_sizes(tibbles)
  cols <- vec_rbind(!!!tibbles)

  list(new = cols, size = sizes, all_one = identical(sizes ,1L))
}

#' @export
summarise2 <- function(.data, ..., .groups = NULL) {
  UseMethod("summarise2")
}

#' @export
summarise2.data.frame <- function(.data, ..., .groups = NULL) {
  cols <- summarise2_cols(.data, ...)
  out <- summarise_build(.data, cols)
  if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, character())
  }
  out
}

#' @export
summarise2.grouped_df <- function(.data, ..., .groups = NULL) {
  cols <- summarise2_cols(.data, ...)
  out <- summarise_build(.data, cols)
  verbose <- summarise_verbose(.groups, caller_env())

  if (is.null(.groups)) {
    if (cols$all_one) {
      .groups <- "drop_last"
    } else {
      .groups <- "keep"
    }
  }

  group_vars <- group_vars(.data)
  if (identical(.groups, "drop_last")) {
    n <- length(group_vars)
    if (n > 1) {
      if (verbose) {
        new_groups <- glue_collapse(paste0("'", group_vars[-n], "'"), sep = ", ")
        summarise_inform("regrouping output by {new_groups}")
      }
      out <- grouped_df(out, group_vars[-n], group_by_drop_default(.data))
    } else {
      if (verbose) {
        summarise_inform("ungrouping output")
      }
    }
  } else if (identical(.groups, "keep")) {
    if (verbose) {
      new_groups <- glue_collapse(paste0("'", group_vars, "'"), sep = ", ")
      summarise_inform("regrouping output by {new_groups}")
    }
    out <- grouped_df(out, group_vars, group_by_drop_default(.data))
  } else if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, group_vars)
  } else if(!identical(.groups, "drop")) {
    abort(c(
      paste0("`.groups` can't be ", as_label(.groups)),
      i = 'Possible values are NULL (default), "drop_last", "drop", "keep", and "rowwise"'
    ))
  }

  out
}

# for now
#' @export
summarise2.rowwise_df <- summarise.rowwise_df
