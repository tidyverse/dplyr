#' Summarise each group to fewer rows
#'
#' @description
#' `summarise()` creates a new data frame. It will have one (or more) rows for
#' each combination of grouping variables; if there are no grouping variables,
#' the output will have a single row summarising all observations in the input.
#' It will contain one column for each grouping variable and one column
#' for each of the summary statistics that you have specified.
#'
#' `summarise()` and `summarize()` are synonyms.
#'
#' @section Useful functions:
#'
#' * Center: [mean()], [median()]
#' * Spread: [sd()], [IQR()], [mad()]
#' * Range: [min()], [max()], [quantile()]
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
#' @param ... <[`data-masking`][dplyr_data_masking]> Name-value pairs of summary
#'   functions. The name will be the name of the variable in the result.
#'
#'   The value can be:
#'
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A vector of length `n`, e.g. `quantile()`.
#'   * A data frame, to add multiple columns from a single expression.
#' @param .groups \Sexpr[results=rd]{lifecycle::badge("experimental")} Grouping structure of the result.
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
#'   * If the number of rows varies, you get "keep".
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
#' # dplyr 1.0.0 allows to summarise to more than one value:
#' mtcars %>%
#'    group_by(cyl) %>%
#'    summarise(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
#'
#' # You use a data frame to create multiple columns so you can wrap
#' # this up into a function:
#' my_quantile <- function(x, probs) {
#'   tibble(x = quantile(x, probs), probs = probs)
#' }
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(my_quantile(disp, c(0.25, 0.75)))
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
summarise <- function(.data, ..., .groups = NULL) {
  UseMethod("summarise")
}
#' @rdname summarise
#' @export
summarize <- summarise

#' @export
summarise.data.frame <- function(.data, ..., .groups = NULL) {
  cols <- summarise_cols(.data, ..., caller_env = caller_env())
  out <- summarise_build(.data, cols)
  if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, character())
  }
  out
}

#' @export
summarise.grouped_df <- function(.data, ..., .groups = NULL) {
  cols <- summarise_cols(.data, ..., caller_env = caller_env())
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
    abort(c(
      paste0("`.groups` can't be ", as_label(.groups)),
      i = 'Possible values are NULL (default), "drop_last", "drop", "keep", and "rowwise"'
    ))
  }

  out
}

#' @export
summarise.rowwise_df <- function(.data, ..., .groups = NULL) {
  cols <- summarise_cols(.data, ..., caller_env = caller_env())
  out <- summarise_build(.data, cols)
  verbose <- summarise_verbose(.groups, caller_env())

  group_vars <- group_vars(.data)
  if (is.null(.groups) || identical(.groups, "keep")) {
    if (verbose) {
      if (length(group_vars)) {
        new_groups <- glue_collapse(paste0("'", group_vars, "'"), sep = ", ")
        summarise_inform("has grouped output by {new_groups}")
      } else {
        summarise_inform("has ungrouped output")
      }
    }
    out <- grouped_df(out, group_vars)
  } else if (identical(.groups, "rowwise")) {
    out <- rowwise_df(out, group_vars)
  } else if (!identical(.groups, "drop")) {
    abort(c(
      paste0("`.groups` can't be ", as_label(.groups)),
      i = 'Possible values are NULL (default), "drop", "keep", and "rowwise"'
    ))
  }

  out
}

summarise_cols <- function(.data, ..., caller_env) {
  mask <- DataMask$new(.data, caller_env)
  old_current_column <- context_peek_bare("column")

  on.exit(context_poke("column", old_current_column), add = TRUE)
  on.exit(mask$forget("summarise"), add = TRUE)

  dots <- dplyr_quosures(...)

  cols <- list()

  sizes <- 1L
  chunks <- vector("list", length(dots))
  types <- vector("list", length(dots))

  chunks <- list()
  types <- list()
  out_names <- character()

  withCallingHandlers({
    for (i in seq_along(dots)) {
      mask$across_cache_reset()
      context_poke("column", old_current_column)

      quosures <- expand_across(dots[[i]])
      quosures_results <- vector(mode = "list", length = length(quosures))

      # with the previous part above, for each element of ... we can
      # have either one or several quosures, each of them handled here:
      for (k in seq_along(quosures)) {
        quo <- quosures[[k]]
        quo_data <- attr(quo, "dplyr:::data")
        if (!is.null(quo_data$column)) {
          context_poke("column", quo_data$column)
        }

        chunks_k <- mask$eval_all_summarise(quo)
        if (is.null(chunks_k)) {
          next
        }

        types_k <- withCallingHandlers(
          vec_ptype_common(!!!chunks_k),
          vctrs_error_incompatible_type = function(cnd) {
            abort(class = "dplyr:::error_summarise_incompatible_combine", parent = cnd)
          }
        )
        chunks_k <- vec_cast_common(!!!chunks_k, .to = types_k)

        quosures_results[[k]] <- list(chunks = chunks_k, types = types_k)
      }

      for (k in seq_along(quosures)) {
        quo <- quosures[[k]]
        quo_data <- attr(quo, "dplyr:::data")

        quo_result <- quosures_results[[k]]
        if (is.null(quo_result)) {
          next
        }
        types_k <- quo_result$types
        chunks_k <- quo_result$chunks

        if (!quo_data$is_named && is.data.frame(types_k)) {
          chunks_extracted <- .Call(dplyr_extract_chunks, chunks_k, types_k)

          walk2(chunks_extracted, names(types_k), function(chunks_k_j, nm) {
            mask$add_one(nm, chunks_k_j)
          })

          chunks <- append(chunks, chunks_extracted)
          types <- append(types, as.list(types_k))
          out_names <- c(out_names, names(types_k))
        } else {
          name <- quo_data$name_auto
          mask$add_one(name, chunks_k)
          chunks <- append(chunks, list(chunks_k))
          types <- append(types, list(types_k))
          out_names <- c(out_names, name)
        }

      }
    }

    recycle_info <- .Call(`dplyr_summarise_recycle_chunks`, chunks, mask$get_rows(), types)
    chunks <- recycle_info$chunks
    sizes <- recycle_info$sizes

    # materialize columns
    for (i in seq_along(chunks)) {
      result <- vec_c(!!!chunks[[i]], .ptype = types[[i]])
      cols[[ out_names[i] ]] <- result
    }

  },
  error = function(e) {
    local_call_step(dots = dots, .index = i, .fn = "summarise",
      .dot_data = inherits(e, "rlang_error_data_pronoun_not_found")
    )
    call_step <- peek_call_step()
    error_name <- call_step$error_name
    show_group_details <- TRUE

    if (inherits(e, "dplyr:::error_summarise_incompatible_combine")) {
      show_group_details <- FALSE
      bullets <- c(
        x = glue("Input `{error_name}` must return compatible vectors across groups", .envir = peek_call_step()),
        i = cnd_bullet_combine_details(e$parent$x, e$parent$x_arg),
        i = cnd_bullet_combine_details(e$parent$y, e$parent$y_arg)
      )
    } else if (inherits(e, "dplyr:::summarise_unsupported_type")) {
      bullets <- c(
        x = glue("Input `{error_name}` must be a vector, not {friendly_type_of(result)}.", result = e$result),
        i = cnd_bullet_rowwise_unlist()
      )
    } else if (inherits(e, "dplyr:::summarise_incompatible_size")) {
      # so that cnd_bullet_cur_group_label() correctly reports the faulty group
      peek_mask()$set_current_group(e$group)

      bullets <- c(
        x = glue("Input `{error_name}` must be size {or_1(expected_size)}, not {size}.", expected_size = e$expected_size, size = e$size),
        i = glue("An earlier column had size {expected_size}.", expected_size = e$expected_size)
      )
    } else if (inherits(e, "dplyr:::summarise_mixed_null")) {
      show_group_details <- FALSE
      bullets <- c(
        x = glue("`{error_name}` must return compatible vectors across groups."),
        i = "Cannot combine NULL and non NULL results."
      )
    } else {
      bullets <- c(
        x = conditionMessage(e)
      )
    }

    bullets <- c(
      cnd_bullet_header("column"),
      i = cnd_bullet_column_info(),
      bullets,
      i = if (show_group_details) cnd_bullet_cur_group_label()
    )
    abort(bullets, class = "dplyr_error")

  })

  list(new = cols, size = sizes, all_one = identical(sizes, 1L))
}

summarise_build <- function(.data, cols) {
  out <- group_keys(.data)
  if (!cols$all_one) {
    out <- vec_slice(out, rep(seq_len(nrow(out)), cols$size))
  }
  dplyr_col_modify(out, cols$new)
}


# messaging ---------------------------------------------------------------

summarise_verbose <- function(.groups, .env) {
  is.null(.groups) &&
    is_reference(topenv(.env), global_env()) &&
    !identical(getOption("dplyr.summarise.inform"), FALSE)
}

summarise_inform <- function(..., .env = parent.frame()) {
  inform(paste0(
    "`summarise()` ", glue(..., .envir = .env), '. You can override using the `.groups` argument.'
  ))
}
