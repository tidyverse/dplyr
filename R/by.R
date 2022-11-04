#' Helper for consistent documentation of `.by`
#'
#' Use `@inheritParams args_by` to consistently document `.by`.
#'
#' @param .by `r lifecycle::badge("experimental")`
#'
#'   <[`tidy-select`][dplyr_tidy_select]> Optionally, select columns to group
#'   by. This grouping will only be active for the duration of this verb.
#'
#'   Can't be used when the data is already a grouped or rowwise data frame.
#'
#' @name args_by
#' @keywords internal
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
    } else {
      type <- "grouped"
      names <- by
      data <- compute_groups(data, by, drop = TRUE)
    }
  }

  new_by(type = type, names = names, data = data)
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
      "{.arg {by_arg}} can't be supplied when ",
      "{.arg {data_arg}} is a grouped data frame."
    )
    cli::cli_abort(message, call = error_call)
  }

  if (is_rowwise_df(data)) {
    message <- paste0(
      "{.arg {by_arg}} can't be supplied when ",
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
