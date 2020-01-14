#' Reduce multiple values down to a single value
#'
#' @description
#' `summarise()` creates a new data frame. It will have one row for each
#' combination of grouping variable; if there are no grouping variables, the
#' output will have a single row summarising all observations in the input.
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
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Name-value pairs of summary
#'   functions. The name will be the name of the variable in the result.
#'   The value should be an expression that returns a single value like
#'   `min(x)`, `n()`, or `sum(is.na(y))`.
#' @family single table verbs
#' @return
#' An object of the same class as `.data`. It will contain one column for
#' each grouping variable and each expression you supply. It will have
#' one row for each combination of the grouping variables.
#'
#' If `.data` is grouped, then the last group will be dropped,
#' e.g.`df %>% group_by(x, y) %>% summarise(n())` will be grouped by
#' `x`. This happens because each group now occupies only a single row.
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
#'
#' # BEWARE: reusing variable names may lead to unexpected results
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(disp = mean(disp), sd = sd(disp))
#'
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' var <- "mass"
#' summarise(starwars, avg = mean(.data[[var]], na.rm = TRUE))
#' # Learn more in ?dplyr_tidy_eval
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
  out[names(cols$new)] <- cols$new
  out
}

#' @export
summarise.grouped_df <- function(.data, ...) {
  out <- NextMethod()

  group_vars <- group_vars(.data)
  if (length(group_vars) > 1) {
    out <- grouped_df(out, group_vars[-length(group_vars)], group_by_drop_default(.data))
  }

  out
}

summarise_cols <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))

  cols <- list()

  .size <- 1L
  # chunks <- vector("list", length(dots))

  tryCatch({

    # generate all chunks and monitor the sizes
    for (i in seq_along(dots)) {
      quo <- dots[[i]]

      # a list in which each element is the result of
      # evaluating the quosure in the "sliced data mask"
      #
      # TODO: reinject hybrid evaluation at the R level
      c(chunks_i, scalar_list) %<-% mask$eval_all_summarise(quo, dots_names, i)

      if (scalar_list) {
        mask$add(auto_named_dots[i], chunks_i)
        cols[[ auto_named_dots[i] ]] <- chunks_i
      } else {
        result <- vec_c(!!!chunks_i)

        if ((is.null(dots_names) || dots_names[i] == "") && is.data.frame(result)) {
          # remember each result separately
          map2(seq_along(result), names(result), function(j, nm) {
            mask$add(nm, pluck(chunks_i, j))
          })
          cols[names(result)] <- result
        } else {
          # remember
          mask$add(auto_named_dots[i], chunks_i)
          cols[[ auto_named_dots[i] ]] <-  result
        }
      }

    }

  },
  vctrs_error_incompatible_type = function(e) {
    stop_summarise_combine(conditionMessage(e), index = i, dots = dots)
  },
  simpleError = function(e) {
    stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
  })

  list(new = cols, size = .size)
}
