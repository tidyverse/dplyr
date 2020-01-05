#' Reduce multiple values down to a single value
#'
#' Create one or more scalar variables summarizing the variables of an
#' existing tbl. Tbls with groups created by [group_by()] will result in one
#' row in the output for each group.  Tbls with no groups will result in one row.
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
#' @return An object of the same class as `.data`. One grouping level will
#'   be dropped.
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
#' # Reusing variable names when summarising may lead to unexpected results
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(disp = mean(disp), sd = sd(disp), double_disp = disp * 2)
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

#' @importFrom tibble add_column
#' @export
summarise.tbl_df <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))

  summaries <- list()

  .size <- 1L

  for (i in seq_along(dots)) {
    # a list in which each element is the result of
    # evaluating the quosure in the "sliced data mask"
    #
    # TODO: reinject hybrid evaluation at the R level
    quo <- dots[[i]]
    chunks <- mask$eval_all_summarise(quo, dots_names, i)

    # check that vec_size() of chunks is compatible with .size
    # and maybe update .size
    .size <- .Call(`dplyr_validate_summarise_sizes`, .size, chunks)

    result <- vec_c(!!!chunks)

    if ((is.null(dots_names) || dots_names[i] == "") && is.data.frame(result)) {
      summaries[names(result)] <- result

      # remember each result separately
      map2(seq_along(result), names(result), function(i, nm) {
        mask$add(nm, pluck(chunks, i))
      })
    } else {
      # treat as a single output otherwise
      summaries[[ auto_named_dots[i] ]] <-  result

      # remember
      mask$add(auto_named_dots[i], chunks)
    }

  }

  grouping <- group_keys(.data)
  if (!identical(.size, 1L)) {
    grouping <- vec_slice(grouping, rep(seq2(1L, nrow(grouping)), .size))
  }

  out <- add_column(grouping, !!!summaries)

  if (is_grouped_df(.data) && length(group_vars(.data)) > 1) {
    out <- grouped_df(out, head(group_vars(.data), -1), group_by_drop_default(.data))
  }

  # copy back attributes
  # TODO: challenge that with some vctrs theory
  atts <- attributes(.data)
  atts <- atts[! names(atts) %in% c("names", "row.names", "groups", "class")]
  for(name in names(atts)) {
    attr(out, name) <- atts[[name]]
  }

  out
}
