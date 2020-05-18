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
#'   * "rowwise": Each row is it's own group.
#'
#'   When `.groups` is not specified, you either get "drop_last"
#'   when all the results are size 1, or "keep" if the size varies.
#'   In addition, a message informs you of that choice, unless the
#'   option "dplyr.summarise.inform" is set to `FALSE`.
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
  cols <- summarise_cols(.data, ...)
  summarise_build(.data, cols)
}

#' @export
summarise.grouped_df <- function(.data, ..., .groups = NULL) {
  cols <- summarise_cols(.data, ...)
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

#' @export
summarise.rowwise_df <- function(.data, ..., .groups = NULL) {
  cols <- summarise_cols(.data, ...)
  out <- summarise_build(.data, cols)
  verbose <- summarise_verbose(.groups, caller_env())

  group_vars <- group_vars(.data)
  if (is.null(.groups) || identical(.groups, "keep")) {
    if (verbose) {
      if (length(group_vars)) {
        new_groups <- glue_collapse(paste0("'", group_vars, "'"), sep = ", ")
        summarise_inform("regrouping output by {new_groups}")
      } else {
        summarise_inform("ungrouping output")
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

hybrid_functions <- env(
  empty_env(),
  mean = function(x, ...) {
    call <- match.call()
    mask <- peek_mask()
    vars <- mask$current_vars()

    stopifnot(is_symbol(call$x) && as_string(call$x) %in% vars)
    if (identical(names(call), c("", "x"))) {
      funs::grouped_mean(x, na.rm = TRUE)
    } else if(identical(names(call), c("", "x", "na.rm"))) {
      na.rm <- call$na.rm
      stopifnot(is_scalar_logical(na.rm))
      funs::grouped_mean(x, na.rm = na.rm)
    }
  },
  n = function() {
    list_sizes(peek_mask()$get_rows())
  }
)


hybrid_eval_summarise <- function(expr, mask) {
  if (is_call(expr, "hybrid")) {
    fn <- new_function(mask$args(), node_cadr(expr))
  } else {
    fn <- new_function(mask$args(), expr, env = hybrid_functions)
  }
  result <- fn()

  if (is.null(result)) {
    result <- fn()
  }
  if (!is.null(result) && !inherits(result, "hybrid_result")) {
    if (vec_size(result) != length(mask$get_rows())) {
      abort("incompatible sizes")
    }
    result <- structure(
      list(x = result),
      class = "hybrid_result"
    )
  }
  result
}

lazy_vec_chop <- function(x, sizes = NULL) {
  structure(list(), x = x, sizes = sizes, class = "dplyr_lazy_vec_chop")
}

summarise_cols <- function(.data, ...) {
  mask <- DataMask$new(.data, caller_env())

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))

  cols <- list()

  chunks <- vector("list", length(dots))
  types <- vector("list", length(dots))
  results <- vector("list", length(dots))
  sizes <- vector("list", length(dots))

  tryCatch({

    # generate all chunks and monitor the sizes
    for (i in seq_along(dots)) {
      quo <- dots[[i]]

      res <- tryCatch(
        hybrid_eval_summarise(quo_get_expr(quo), mask),
        error = function(cnd) {
          NULL
        }
      )
      mask$across_cache_reset()
      standard <- is.null(res)

      if (standard) {
        # no result from hybrid, so proceed with standard
        # evaluation to get the chunks
        chunks[[i]] <- mask$eval_all_summarise(quo)
        types[[i]] <- tryCatch(
          vec_ptype_common(!!!chunks[[i]]),
          vctrs_error_incompatible_type = function(cnd) {
            abort(class = "dplyr:::error_summarise_incompatible_combine", parent = cnd)
        })
      } else {
        # hybrid evaluation was successfull, so we have the result
        # and we can deduce the type
        #
        # we however don't have the chunks, but we might never need them
        # so we'll make them lazily later
        results[[i]] <- res$x
        types[[i]] <- vec_ptype(res$x)
        sizes[i] <- list(res$sizes)
      }

      # adding the chunks to the mask
      type <- types[[i]]
      not_named <- is.null(dots_names) || dots_names[i] == ""
      if (not_named && is.data.frame(type)) {
        # remember each result separately
        map2(seq_along(type), names(type), function(j, nm) {
          slices <- if(standard) {
            pluck(chunks[[i]], j)
          } else {
            lazy_vec_chop(res$x[[j]], res$sizes)
          }
          mask$set(nm, slices)
        })
      } else {
        # remember
        if (standard) {
          mask$set(auto_named_dots[i], chunks[[i]])
        } else {
          mask$set(auto_named_dots[i], lazy_vec_chop(res$x, res$sizes))
        }
      }

    }

    recycle_info <- .Call(`dplyr_summarise_recycle_chunks`, chunks, mask$get_rows(), types, results, sizes)
    chunks <- recycle_info$chunks
    sizes <- recycle_info$sizes
    results <- recycle_info$results

    # materialize columns
    for (i in seq_along(dots)) {
      result <- results[[i]] %||% vec_c(!!!chunks[[i]], .ptype = types[[i]])

      if ((is.null(dots_names) || dots_names[i] == "") && is.data.frame(result)) {
        cols[names(result)] <- result
      } else {
        cols[[ auto_named_dots[i] ]] <- result
      }
    }

  },
    error = function(e) {
      if (inherits(e, "rlang_error_data_pronoun_not_found")) {
        stop_error_data_pronoun_not_found(conditionMessage(e), index = i, dots = dots, fn = "summarise")
      } else if (inherits(e, "dplyr:::error_summarise_incompatible_combine")) {
        stop_combine(e$parent, index = i, dots = dots, fn = "summarise")
      } else if (inherits(e, "dplyr:::summarise_unsupported_type")) {
        stop_summarise_unsupported_type(result = e$result, index = i, dots = dots)
      } else if (inherits(e, "dplyr:::summarise_incompatible_size")) {
        stop_summarise_incompatible_size(size = e$size, group = e$group, index = e$index, expected_size = e$expected_size, dots = dots)
      } else {
        stop_dplyr(i, dots, fn = "summarise", problem = conditionMessage(e), parent = e)
      }
    })

  list(new = cols, size = sizes, all_one = identical(sizes ,1L))
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
    "`summarise()` ", glue(..., .envir = .env), " (override with `.groups` argument)"
  ))
}
