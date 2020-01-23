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
#' @family single table verbs
#' @return
#' An object _usually_ of the same type as `.data`.
#'
#' * The rows come from the underlying `group_keys()`.
#' * The columns are a combination of the grouping keys and the summary
#'   expressions that you provide.
#' * If `x` is grouped by more than one variable, the output will be another
#'   [grouped_df] with the right-most group removed.
#' * If `x` is grouped by one variable, or is not grouped, the output will
#'   be a [tibble].
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
summarise <- function(.data, ...) {
  UseMethod("summarise")
}
#' @rdname summarise
#' @export
summarize <- summarise

#' @export
summarise.data.frame <- function(.data, ...) {
  cols <- summarise_cols(.data, ...)

  out <- group_keys(.data)
  if (!identical(cols$size, 1L)) {
    out <- vec_slice(out, rep(1:nrow(out), cols$size))
  }

  dplyr_col_modify(out, cols$new)
}

#' @export
summarise.grouped_df <- function(.data, ...) {
  cols <- summarise_cols(.data, ...)

  out <- group_keys(.data)
  if (!identical(cols$size, 1L)) {
    out <- vec_slice(out, rep(1:nrow(out), cols$size))
  }
  out <- dplyr_col_modify(out, cols$new)

  group_vars <- group_vars(.data)
  n <- length(group_vars)
  if (n <= 1) {
    return(out)
  }

  # Figure out rows of old groups
  if (identical(cols$size, 1L)) {
    rows <- as.list(1:nrow(out))
  } else {
    breaks <- cumsum(c(1L, cols$size))
    start <- breaks[-length(breaks)]
    end <- breaks[-1] - 1L
    rows <- map2(start, end, seq2)
  }
  # If needed, collapse old groups into new groups
  groups <- group_keys(.data)
  groups <- groups[setdiff(names(groups), group_vars[[n]])]
  loc <- vec_group_loc(groups)

  out_groups <- loc$key
  out_groups$.rows <- as_list_of(lapply(loc$loc, function(i) unlist(rows[i])) %||% integer(), .ptype = integer())

  new_grouped_df(out, out_groups)
}

#' @export
summarise.rowwise_df <- function(.data, ...) {
  out <- NextMethod()
  rowwise_df(out, group_vars(.data))
}

summarise_cols <- function(.data, ...) {
  mask <- DataMask$new(.data, caller_env())

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))

  cols <- list()

  sizes <- 1L
  chunks <- vector("list", length(dots))

  tryCatch({

    # generate all chunks and monitor the sizes
    for (i in seq_along(dots)) {
      quo <- dots[[i]]

      # a list in which each element is the result of
      # evaluating the quosure in the "sliced data mask"
      #
      # TODO: reinject hybrid evaluation at the R level
      chunks[[i]] <- mask$eval_all_summarise(quo)

      result_type <- vec_ptype_common(!!!chunks[[i]])

      if ((is.null(dots_names) || dots_names[i] == "") && is.data.frame(result_type)) {
        # remember each result separately
        map2(seq_along(result_type), names(result_type), function(j, nm) {
          mask$add(nm, pluck(chunks[[i]], j))
        })
      } else {
        # remember
        mask$add(auto_named_dots[i], chunks[[i]])
      }
    }

    c(chunks, sizes) %<-% .Call(`dplyr_summarise_recycle_chunks`, chunks)

    # materialize columns
    for (i in seq_along(dots)) {
      result <- vec_c(!!!chunks[[i]])

      if ((is.null(dots_names) || dots_names[i] == "") && is.data.frame(result)) {
        cols[names(result)] <- result
      } else {
        cols[[ auto_named_dots[i] ]] <-  result
      }
    }

  },
  rlang_error_data_pronoun_not_found = function(e) {
    stop_error_data_pronoun_not_found(conditionMessage(e), index = i, dots = dots, fn = "summarise")
  },
  vctrs_error_incompatible_type = function(e) {
    stop_combine(e, index = i, dots = dots, fn = "summarise")
  },
  dplyr_summarise_unsupported_type = function(cnd) {
    stop_summarise_unsupported_type(result = cnd$result, index = i, dots = dots)
  },
  dplyr_summarise_incompatible_size = function(cnd) {
    stop_summarise_incompatible_size(size = cnd$size, group = cnd$group, index = cnd$index, expected_size = cnd$expected_size, dots = dots)
  },
  simpleError = function(e) {
    stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
  })

  list(new = cols, size = sizes)
}
