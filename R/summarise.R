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
#' @export
#' @inheritParams arrange
#' @inheritParams args_by
#' @param ... <[`data-masking`][dplyr_data_masking]> Name-value pairs of summary
#'   functions. The name will be the name of the variable in the result.
#'
#'   The value can be:
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A data frame, to add multiple columns from a single expression.
#'
#'   `r lifecycle::badge("deprecated")` Returning values with size 0 or >1 was
#'   deprecated as of 1.1.0. Please use [reframe()] for this instead.
#' @param .groups `r lifecycle::badge("experimental")` Grouping structure of the
#'   result.
#'
#'   * "drop_last": dropping the last level of grouping. This was the
#'   only supported option before version 1.0.0.
#'   * "drop": All levels of grouping are dropped.
#'   * "keep": Same grouping structure as `.data`.
#'   * "rowwise": Each row is its own group.
#'
#'   When `.groups` is not specified, it is chosen
#'   based on the number of rows of the results:
#'   * If all the results have 1 row, you get "drop_last".
#'   * If the number of rows varies, you get "keep" (note that returning a
#'     variable number of rows was deprecated in favor of [reframe()], which
#'     also unconditionally drops all levels of grouping).
#'
#'   In addition, a message informs you of that choice, unless the result is ungrouped,
#'   the option "dplyr.summarise.inform" is set to `FALSE`,
#'   or when `summarise()` is called from a function in a package.
#'
#' @family single table verbs
#' @return
#' An object _usually_ of the same type as `.data`.
#'
#' * The rows come from the underlying [group_keys()].
#' * The columns are a combination of the grouping keys and the summary
#'   expressions that you provide.
#' * The grouping structure is controlled by the `.groups=` argument, the
#'   output may be another [grouped_df], a [tibble] or a [rowwise] data frame.
#' * Data frame attributes are **not** preserved, because `summarise()`
#'   fundamentally creates a new data frame.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("summarise")}.
#' @examples
#' # A summary applied to ungrouped tbl returns a single row
#' mtcars %>%
#'   summarise(mean = mean(disp), n = n())
#'
#' # Usually, you'll want to group first
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(mean = mean(disp), n = n())
#'
#' # Each summary call removes one grouping level (since that group
#' # is now just a single row)
#' mtcars %>%
#'   group_by(cyl, vs) %>%
#'   summarise(cyl_n = n()) %>%
#'   group_vars()
#'
#' # BEWARE: reusing variables may lead to unexpected results
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(disp = mean(disp), sd = sd(disp))
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' var <- "mass"
#' summarise(starwars, avg = mean(.data[[var]], na.rm = TRUE))
#' # Learn more in ?dplyr_data_masking
#'
#' # In dplyr 1.1.0, returning multiple rows per group was deprecated in favor
#' # of `reframe()`, which never messages and always returns an ungrouped
#' # result:
#' mtcars %>%
#'    group_by(cyl) %>%
#'    summarise(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
#' # ->
#' mtcars %>%
#'    group_by(cyl) %>%
#'    reframe(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
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
  out <- summarise_build(by, cols)

  if (!cols$all_one) {
    summarise_deprecate_variable_size()
  }

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
  out <- summarise_build(by, cols)
  verbose <- summarise_verbose(.groups, caller_env())

  if (!cols$all_one) {
    summarise_deprecate_variable_size()
  }

  if (is.null(.groups)) {
    if (cols$all_one) {
      .groups <- "drop_last"
    } else {
      .groups <- "keep"
    }
  }

  group_vars <- by$names
  if (identical(.groups, "drop_last")) {
    n <- length(group_vars)
    if (n > 1) {
      if (verbose) {
        new_groups <- glue_collapse(paste0("'", group_vars[-n], "'"), sep = ", ")
        summarise_inform("has grouped output by {new_groups}")
      }
      out <- grouped_df(out, group_vars[-n], group_by_drop_default(.data))
    }
  } else if (identical(.groups, "keep")) {
    if (verbose) {
      new_groups <- glue_collapse(paste0("'", group_vars, "'"), sep = ", ")
      summarise_inform("has grouped output by {new_groups}")
    }
    out <- grouped_df(out, group_vars, group_by_drop_default(.data))
  } else if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, group_vars)
  } else if(!identical(.groups, "drop")) {
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
  out <- summarise_build(by, cols)
  verbose <- summarise_verbose(.groups, caller_env())

  if (!cols$all_one) {
    summarise_deprecate_variable_size()
  }

  group_vars <- by$names
  if (is.null(.groups) || identical(.groups, "keep")) {
    if (verbose && length(group_vars)) {
      new_groups <- glue_collapse(paste0("'", group_vars, "'"), sep = ", ")
      summarise_inform("has grouped output by {new_groups}")
    }
    out <- grouped_df(out, group_vars)
  } else if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, group_vars)
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

  old_current_column <- context_peek_bare("column")
  on.exit(context_poke("column", old_current_column), add = TRUE)

  warnings_state <- env(warnings = list())

  cols <- list()

  sizes <- 1L
  chunks <- list()
  results <- list()
  types <- list()
  out_names <- character()

  local_error_context(dots, 0L, mask = mask)

  withCallingHandlers({
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
              name   = types_k_names[j],
              chunks = chunks_extracted[[j]],
              result  = results_k[[j]]
            )
          }

          chunks <- append(chunks, chunks_extracted)
          types <- append(types, as.list(types_k))
          results <- append(results, results_k)
          out_names <- c(out_names, types_k_names)
        } else {
          name <- quo_data$name_auto
          mask$add_one(name = name, chunks = chunks_k, result = results_k)
          chunks <- append(chunks, list(chunks_k))
          types <- append(types, list(types_k))
          results <- append(results, list(results_k))
          out_names <- c(out_names, name)
        }
      }
    }

    # Recycle horizontally across sets of chunks.
    # Modifies `chunks` and `results` in place for efficiency!
    sizes <- .Call(`dplyr_summarise_recycle_chunks_in_place`, chunks, results)

    # Materialize columns, regenerate any `results` that were `NULL`ed
    # during the recycling process.
    for (i in seq_along(chunks)) {
      result <- results[[i]] %||% vec_c(!!!chunks[[i]], .ptype = types[[i]])
      cols[[ out_names[i] ]] <- result
    }
  },
  error = function(cnd) {
    if (inherits(cnd, "dplyr:::summarise_incompatible_size")) {
      action <- "recycle"
      i <- cnd$dplyr_error_data$index
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
  ))

  signal_warnings(warnings_state, error_call)

  list(new = cols, sizes = sizes, all_one = all(sizes == 1L))
}

summarise_eval_one <- function(quo, mask) {
  quo_data <- attr(quo, "dplyr:::data")
  if (!is.null(quo_data$column)) {
    context_poke("column", quo_data$column)

    # wrap the error when this has been expanded
    chunks_k <- withCallingHandlers(
      mask$eval_all_summarise(quo),
      error = function(cnd) {
        msg <- glue("Can't compute column `{quo_data$name_auto}`.")
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

  types_k <- dplyr_vec_ptype_common(chunks_k, quo_data$name_auto)

  chunks_k <- vec_cast_common(!!!chunks_k, .to = types_k)
  result_k <- vec_c(!!!chunks_k, .ptype = types_k)
  list(chunks = chunks_k, types = types_k, results = result_k)
}

summarise_build <- function(by, cols) {
  out <- group_keys0(by$data)
  if (!cols$all_one) {
    out <- vec_rep_each(out, cols$sizes)
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
  expected_size <- cnd$dplyr_error_data$expected_size
  size          <- cnd$dplyr_error_data$size
  group         <- cnd$dplyr_error_data$group

  error_context <- peek_error_context()
  error_name <- ctxt_error_label(error_context)

  # FIXME: So that cnd_bullet_cur_group_label() correctly reports the
  # faulty group
  peek_mask()$set_current_group(group)

  c(
    glue("`{error_name}` must be size {or_1(expected_size)}, not {size}."),
    i = glue("An earlier column had size {expected_size}.")
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

summarise_inform <- function(..., .env = parent.frame()) {
  inform(paste0(
    "`summarise()` ", glue(..., .envir = .env), '. You can override using the `.groups` argument.'
  ))
}

summarise_deprecate_variable_size <- function(env = caller_env(),
                                              user_env = caller_env(2)) {
  lifecycle::deprecate_warn(
    when = "1.1.0",
    what = I("Returning more (or less) than 1 row per `summarise()` group"),
    with = "reframe()",
    details = paste0(
      "When switching from `summarise()` to `reframe()`, remember that ",
      "`reframe()` always returns an ungrouped data frame and adjust accordingly."
    ),
    env = env,
    user_env = user_env,
    always = TRUE
  )
}
