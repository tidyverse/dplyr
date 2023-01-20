#' Helper for consistent documentation of `.by`
#'
#' Use `@inheritParams args_by` to consistently document `.by`.
#'
#' @param .by `r lifecycle::badge("experimental")`
#'
#'   <[`tidy-select`][dplyr_tidy_select]> Optionally, a selection of columns to
#'   group by for just this operation, functioning as an alternative to [group_by()]. For
#'   details and examples, see [?dplyr_by][dplyr_by].
#'
#' @name args_by
#' @keywords internal
NULL

#' Per-operation grouping with `.by`/`by`
#'
#' ```{r, echo = FALSE, results = "asis"}
#' result <- rlang::with_options(
#'   knitr::knit_child("man/rmd/by.Rmd"),
#'   dplyr.summarise.inform = TRUE
#' )
#' cat(result, sep = "\n")
#' ```
#'
#' @name dplyr_by
NULL

compute_by <- function(by,
                       data,
                       ...,
                       by_arg = "by",
                       data_arg = "data",
                       error_call = caller_env()) {
  check_dots_empty0(...)

  error_call <- dplyr_error_call(error_call)

  by <- enquo(by)
  check_by(by, data, by_arg = by_arg, data_arg = data_arg, error_call = error_call)

  if (is_grouped_df(data)) {
    type <- "grouped"
    names <- group_vars(data)
    data <- group_data(data)
  } else if (is_rowwise_df(data)) {
    type <- "rowwise"
    names <- group_vars(data)
    data <- group_data(data)
  } else {
    by <- eval_select_by(by, data, error_call = error_call)

    if (length(by) == 0L) {
      # `by = NULL` or empty selection
      type <- "ungrouped"
      names <- by
      data <- group_data(data)
      data <- as_tibble(data)
    } else {
      type <- "grouped"
      names <- by
      data <- compute_by_groups(data, by, error_call = error_call)
    }
  }

  new_by(type = type, names = names, data = data)
}

compute_by_groups <- function(data, names, error_call = caller_env()) {
  data <- dplyr_col_select(data, names, error_call = error_call)
  info <- vec_group_loc(data)

  size <- vec_size(info)

  out <- dplyr_new_list(info$key)
  out[[".rows"]] <- new_list_of(info$loc, ptype = integer())
  out <- new_tibble(out, nrow = size)

  out
}

check_by <- function(by,
                     data,
                     ...,
                     by_arg = "by",
                     data_arg = "data",
                     error_call = caller_env()) {
  check_dots_empty0(...)

  if (quo_is_null(by)) {
    return(invisible(NULL))
  }

  if (is_grouped_df(data)) {
    message <- paste0(
      "Can't supply {.arg {by_arg}} when ",
      "{.arg {data_arg}} is a grouped data frame."
    )
    cli::cli_abort(message, call = error_call)
  }

  if (is_rowwise_df(data)) {
    message <- paste0(
      "Can't supply {.arg {by_arg}} when ",
      "{.arg {data_arg}} is a rowwise data frame."
    )
    cli::cli_abort(message, call = error_call)
  }

  invisible(NULL)
}

eval_select_by <- function(by,
                           data,
                           error_call = caller_env()) {
  out <- tidyselect::eval_select(
    expr = by,
    data = data,
    allow_rename = FALSE,
    error_call = error_call
  )
  names(out)
}

new_by <- function(type, names, data) {
  structure(list(type = type, names = names, data = data), class = "dplyr_by")
}

check_by_typo <- function(...,
                          by = NULL,
                          error_call = caller_env()) {
  check_by_typo_impl(
    wrong = "by",
    right = ".by",
    by = {{ by }},
    error_call = error_call
  )
}
check_dot_by_typo <- function(...,
                              .by = NULL,
                              error_call = caller_env()) {
  check_by_typo_impl(
    wrong = ".by",
    right = "by",
    by = {{ .by }},
    error_call = error_call
  )
}
check_by_typo_impl <- function(wrong,
                               right,
                               by = NULL,
                               error_call = caller_env()) {
  by <- enquo(by)

  if (quo_is_null(by)) {
    return(invisible())
  }

  message <- c(
    "Can't specify an argument named {.code {wrong}} in this verb.",
    i = "Did you mean to use {.code {right}} instead?"
  )

  cli::cli_abort(message, call = error_call)
}
