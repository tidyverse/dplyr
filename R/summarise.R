#' Summarise each group down to one row
#'
#' @description
#' `summarise()` creates a new data frame. It returns one row for each
#' combination of grouping variables; if there are no grouping variables, the
#' output will have a single row summarising all observations in the input. It
#' will contain one column for each grouping variable and one column for each of
#' the summary statistics that you have specified.
#'
#' `summarise()` and `summarize()` are synonyms.
#'
#' @section Useful functions:
#'
#' * Center: [mean()], [median()]
#' * Spread: [sd()], [IQR()], [mad()]
#' * Range: [min()], [max()],
#' * Position: [first()], [last()], [nth()],
#' * Count: [n()], [n_distinct()]
#' * Logical: [any()], [all()]
#'
#' @section Backend variations:
#'
#' The data frame backend supports creating a variable and using it in the
#' same summary. This means that previously created summary variables can be
#' further transformed or combined within the summary, as in [mutate()].
#' However, it also means that summary variables with the same names as previous
#' variables overwrite them, making those variables unavailable to later summary
#' variables.
#'
#' This behaviour may not be supported in other backends. To avoid unexpected
#' results, consider using new names for your summary variables, especially when
#' creating multiple summaries.
#'
#' @inheritParams arrange
#' @inheritParams args_by
#'
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs of
#'   summary functions. The name will be the name of the variable in the result.
#'
#'   The value can be:
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A data frame with 1 row, to add multiple columns from a single expression.
#'
#' @param .groups `r lifecycle::badge("experimental")` Grouping structure of the
#'   result.
#'
#'   * `"drop_last"`: drops the last level of grouping. This was the
#'   only supported option before version 1.0.0.
#'   * `"drop"`: All levels of grouping are dropped.
#'   * `"keep"`: Same grouping structure as `.data`.
#'   * `"rowwise"`: Each row is its own group.
#'
#'   When `.groups` is not specified, it is set to `"drop_last"` for a grouped
#'   data frame, and `"keep"` for a rowwise data frame. In addition, a message
#'   informs you of how the result will be grouped unless the result is
#'   ungrouped, the option `"dplyr.summarise.inform"` is set to `FALSE`, or when
#'   `summarise()` is called from a function in a package.
#'
#' @returns
#' An object _usually_ of the same type as `.data`.
#'
#' * The rows come from the underlying [group_keys()].
#' * The columns are a combination of the grouping keys and the summary
#'   expressions that you provide.
#' * The grouping structure is controlled by the `.groups=` argument, the
#'   output may be another [grouped_df], a [tibble] or a [rowwise] data frame.
#' * Data frame attributes are **not** preserved, because `summarise()`
#'   fundamentally creates a new data frame.
#'
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("summarise")}.
#'
#' @family single table verbs
#' @export
#' @examples
#' # A summary applied to ungrouped tbl returns a single row
#' mtcars |>
#'   summarise(mean = mean(disp), n = n())
#'
#' # Usually, you'll want to group first
#' mtcars |>
#'   group_by(cyl) |>
#'   summarise(mean = mean(disp), n = n())
#'
#' # Each summary call removes one grouping level (since that group
#' # is now just a single row)
#' mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(cyl_n = n()) |>
#'   group_vars()
#'
#' # BEWARE: reusing variables may lead to unexpected results
#' mtcars |>
#'   group_by(cyl) |>
#'   summarise(disp = mean(disp), sd = sd(disp))
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' var <- "mass"
#' summarise(starwars, avg = mean(.data[[var]], na.rm = TRUE))
#' # Learn more in ?rlang::args_data_masking
summarise <- function(.data, ..., .by = NULL, .groups = NULL) {
  by <- enquo(.by)

  if (!quo_is_null(by) && !is.null(.groups)) {
    abort("Can't supply both `.by` and `.groups`.")
  }

  UseMethod("summarise")
}
#' @rdname summarise
#' @export
summarize <- summarise

#' @export
summarise.data.frame <- function(.data, ..., .by = NULL, .groups = NULL) {
  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")

  cols <- summarise_cols(.data, dplyr_quosures(...), by, "summarise")
  out <- summarise_build(by, cols, "summarise")

  if (!is_tibble(.data)) {
    # The `by` group data we build from is always a tibble,
    # so we have to manually downcast as needed
    out <- as.data.frame(out)
  }

  if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, character())
  }

  out
}

#' @export
summarise.grouped_df <- function(.data, ..., .by = NULL, .groups = NULL) {
  # Will always error if `.by != NULL` b/c you can't use it with grouped/rowwise dfs.
  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")

  cols <- summarise_cols(.data, dplyr_quosures(...), by, "summarise")
  out <- summarise_build(by, cols, "summarise")
  verbose <- summarise_verbose(.groups, caller_env())

  if (is.null(.groups)) {
    .groups <- "drop_last"
  }

  old_groups <- by$names
  if (identical(.groups, "drop_last")) {
    n <- length(old_groups)
    if (n > 1) {
      new_groups <- old_groups[-n]
      if (verbose) {
        inform_implicit_drop_last_for_grouped_df(old_groups, new_groups)
      }
      out <- grouped_df(out, new_groups, group_by_drop_default(.data))
    }
  } else if (identical(.groups, "keep")) {
    out <- grouped_df(out, old_groups, group_by_drop_default(.data))
  } else if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, old_groups)
  } else if (!identical(.groups, "drop")) {
    bullets <- c(
      paste0("`.groups` can't be ", as_label(.groups)),
      i = 'Possible values are NULL (default), "drop_last", "drop", "keep", and "rowwise"'
    )
    abort(bullets)
  }

  out
}

#' @export
summarise.rowwise_df <- function(.data, ..., .by = NULL, .groups = NULL) {
  # Will always error if `.by != NULL` b/c you can't use it with grouped/rowwise dfs.
  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")

  cols <- summarise_cols(.data, dplyr_quosures(...), by, "summarise")
  out <- summarise_build(by, cols, "summarise")
  verbose <- summarise_verbose(.groups, caller_env())

  if (is.null(.groups)) {
    .groups <- "keep"
  }

  old_groups <- by$names
  if (identical(.groups, "keep")) {
    if (verbose && length(old_groups) > 0L) {
      inform_implicit_keep_for_rowwise_df(old_groups)
    }
    out <- grouped_df(out, old_groups)
  } else if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, old_groups)
  } else if (!identical(.groups, "drop")) {
    bullets <- c(
      paste0("`.groups` can't be ", as_label(.groups)),
      i = 'Possible values are NULL (default), "drop", "keep", and "rowwise"'
    )
    abort(bullets)
  }

  out
}

summarise_cols <- function(data, dots, by, verb, error_call = caller_env()) {
  error_call <- dplyr_error_call(error_call)

  mask <- DataMask$new(data, by, verb, error_call = error_call)
  on.exit(mask$forget(), add = TRUE)

  n_groups <- mask$get_n_groups()

  old_current_column <- context_peek_bare("column")
  on.exit(context_poke("column", old_current_column), add = TRUE)

  warnings_state <- env(warnings = list())

  chunks <- list()
  types <- list()
  results <- list()
  out_names <- character()

  local_error_context(dots, 0L, mask = mask)

  withCallingHandlers(
    {
      for (i in seq_along(dots)) {
        poke_error_context(dots, i, mask = mask)
        context_poke("column", old_current_column)

        dot <- dots[[i]]

        # - expand
        dot <- expand_pick(dot, mask)
        quosures <- expand_across(dot)

        # - compute
        quosures_results <- map(quosures, summarise_eval_one, mask = mask)

        # - structure
        for (k in seq_along(quosures)) {
          quo <- quosures[[k]]
          quo_data <- attr(quo, "dplyr:::data")

          quo_result <- quosures_results[[k]]
          if (is.null(quo_result)) {
            next
          }
          types_k <- quo_result$types
          chunks_k <- quo_result$chunks
          results_k <- quo_result$results

          if (!quo_data$is_named && is.data.frame(types_k)) {
            chunks_extracted <- .Call(dplyr_extract_chunks, chunks_k, types_k)
            types_k_names <- names(types_k)
            for (j in seq_along(chunks_extracted)) {
              mask$add_one(
                name = types_k_names[j],
                chunks = chunks_extracted[[j]],
                result = results_k[[j]]
              )
            }

            chunks <- append(chunks, chunks_extracted)
            types <- append(types, as.list(types_k))
            results <- append(results, results_k)
            out_names <- c(out_names, types_k_names)
          } else {
            name <- dplyr_quosure_name(quo_data)
            mask$add_one(name = name, chunks = chunks_k, result = results_k)
            chunks <- append(chunks, list(chunks_k))
            types <- append(types, list(types_k))
            results <- append(results, list(results_k))
            out_names <- c(out_names, name)
          }
        }
      }

      if (verb == "summarise") {
        # For `summarise()`, check that all results are size 1.
        .Call(`dplyr_summarise_check_all_size_one`, chunks, n_groups)
        group_sizes <- NULL
      } else {
        # For `reframe()`, recycle horizontally across expressions within a
        # single group. Modifies `chunks` and `results` in place for efficiency!
        group_sizes <- .Call(
          `dplyr_reframe_recycle_horizontally_in_place`,
          chunks,
          results,
          n_groups
        )

        # Regenerate any `results` that were `NULL`ed in place during the
        # recycling process due to recycling of `chunks` changing the size
        for (i in seq_along(chunks)) {
          if (is.null(results[[i]])) {
            results[[i]] <- vec_c(!!!chunks[[i]], .ptype = types[[i]])
          }
        }
      }
    },
    error = function(cnd) {
      if (inherits(cnd, "dplyr:::reframe_incompatible_size")) {
        action <- "recycle"
        i <- cnd$dplyr_error_data$index_expression
      } else if (inherits(cnd, "dplyr:::summarise_incompatible_size")) {
        action <- "compute"
        i <- cnd$dplyr_error_data$index_expression
      } else {
        action <- "compute"
        i <- i
      }
      handler <- dplyr_error_handler(
        dots = dots,
        mask = mask,
        bullets = summarise_bullets,
        error_call = error_call,
        action = action
      )
      handler(cnd)
    },
    warning = dplyr_warning_handler(
      state = warnings_state,
      mask = mask,
      error_call = error_call
    )
  )

  # Build output `cols`, assigning by name so `summarise(df, a = expr, a = expr)`
  # only retains the 2nd assignment
  cols <- list()
  for (i in seq_along(results)) {
    cols[[out_names[i]]] <- results[[i]]
  }

  signal_warnings(warnings_state, error_call)

  list(new = cols, group_sizes = group_sizes)
}

summarise_eval_one <- function(quo, mask) {
  quo_data <- attr(quo, "dplyr:::data")
  if (!is.null(quo_data$column)) {
    context_poke("column", quo_data$column)

    # wrap the error when this has been expanded
    chunks_k <- withCallingHandlers(
      mask$eval_all_summarise(quo),
      error = function(cnd) {
        name <- dplyr_quosure_name(quo_data)
        msg <- glue("Can't compute column `{name}`.")
        abort(msg, call = call("across"), parent = cnd)
      }
    )
  } else {
    # no wrapping otherwise
    chunks_k <- mask$eval_all_summarise(quo)
  }
  if (is.null(chunks_k)) {
    return(NULL)
  }

  # `name` specified lazily
  types_k <- dplyr_vec_ptype_common(
    chunks = chunks_k,
    name = dplyr_quosure_name(quo_data)
  )

  chunks_k <- vec_cast_common(!!!chunks_k, .to = types_k)
  result_k <- vec_c(!!!chunks_k, .ptype = types_k)
  list(chunks = chunks_k, types = types_k, results = result_k)
}

summarise_build <- function(by, cols, verb) {
  out <- group_keys0(by$data)
  if (verb == "reframe") {
    # Repeat keys for `reframe()`
    out <- vec_rep_each(out, cols$group_sizes)
  }
  dplyr_col_modify(out, cols$new)
}

summarise_bullets <- function(cnd, ...) {
  UseMethod("summarise_bullets")
}

#' @export
`summarise_bullets.dplyr:::summarise_unsupported_type` <- function(cnd, ...) {
  result <- cnd$dplyr_error_data$result
  error_name <- ctxt_error_label()
  c(
    glue("`{error_name}` must be a vector, not {obj_type_friendly(result)}."),
    i = cnd_bullet_rowwise_unlist()
  )
}

#' @export
`summarise_bullets.dplyr:::summarise_incompatible_size` <- function(cnd, ...) {
  index_group <- cnd$dplyr_error_data$index_group
  actual_size <- cnd$dplyr_error_data$actual_size

  error_context <- peek_error_context()
  error_name <- ctxt_error_label(error_context)

  # FIXME: So that cnd_bullet_cur_group_label() correctly reports the
  # faulty group
  peek_mask()$set_current_group(index_group)

  c(
    cli::format_inline(
      "{.code {error_name}} must be size 1, not {actual_size}."
    ),
    i = cli::format_inline(
      "To return more or less than 1 row per group, use {.fn reframe}."
    )
  )
}

#' @export
`summarise_bullets.dplyr:::reframe_incompatible_size` <- function(cnd, ...) {
  index_group <- cnd$dplyr_error_data$index_group
  actual_size <- cnd$dplyr_error_data$actual_size
  expected_size <- cnd$dplyr_error_data$expected_size

  error_context <- peek_error_context()
  error_name <- ctxt_error_label(error_context)

  # FIXME: So that cnd_bullet_cur_group_label() correctly reports the
  # faulty group
  peek_mask()$set_current_group(index_group)

  c(
    cli::format_inline(
      "{.code {error_name}} must be size {or_1(expected_size)}, not {actual_size}."
    ),
    i = cli::format_inline(
      "An earlier column had size {expected_size}."
    )
  )
}

#' @export
`summarise_bullets.dplyr:::summarise_mixed_null` <- function(cnd, ...) {
  error_name <- ctxt_error_label()
  c(
    glue("`{error_name}` must return compatible vectors across groups."),
    x = "Can't combine NULL and non NULL results."
  )
}

# messaging ---------------------------------------------------------------

summarise_verbose <- function(.groups, .env) {
  if (!is.null(.groups)) {
    # User supplied `.groups`
    return(FALSE)
  }

  inform <- getOption("dplyr.summarise.inform")

  if (is_true(inform) || is_false(inform)) {
    # User supplied global option
    return(inform)
  }

  is_reference(topenv(.env), global_env())
}

inform_implicit_drop_last_for_grouped_df <- function(old, new) {
  # Only going to show this message if `length(old) > 1`, so don't need to
  # worry about the length 0 or length 1 cases.
  by <- paste0("c(", paste0(old, collapse = ", "), ")")

  inform(cli_format_each_inline(
    "{.fn summarise} has regrouped the output.",
    i = "Summaries were computed grouped by {cli::col_blue(old)}.",
    i = "Output is grouped by {cli::col_blue(new)}.",
    i = "Use {.code summarise(.groups = \"drop_last\")} to silence this message.",
    i = "Use {.code summarise(.by = {by})} for {.topic [per-operation grouping](dplyr::dplyr_by)} instead."
  ))
}

inform_implicit_keep_for_rowwise_df <- function(groups) {
  inform(cli_format_each_inline(
    "{.fn summarise} has converted the output from a rowwise data frame to a grouped data frame.",
    i = "Summaries were computed rowwise.",
    i = "Output is grouped by {cli::col_blue(groups)}.",
    i = "Use {.code summarise(.groups = \"keep\")} to silence this message."
  ))
}
