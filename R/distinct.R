#' Select distinct/unique rows
#'
#' Retain only unique/distinct rows from an input tbl. This is similar
#' to [unique.data.frame()], but considerably faster.
#'
#' Comparing list columns is not fully supported.
#' Elements in list columns are compared by reference.
#' A warning will be given when trying to include list columns in the
#' computation.
#' This behavior is kept for compatibility reasons and may change in a future
#' version.
#' See examples.
#'
#' @param .data a tbl
#' @param ... Optional variables to use when determining uniqueness. If there
#'   are multiple rows for a given combination of inputs, only the first
#'   row will be preserved. If omitted, will use all variables.
#' @param .keep_all If `TRUE`, keep all variables in `.data`.
#'   If a combination of `...` is not distinct, this keeps the
#'   first row of values.
#' @inheritParams filter
#' @export
#' @examples
#' df <- tibble(
#'   x = sample(10, 100, rep = TRUE),
#'   y = sample(10, 100, rep = TRUE)
#' )
#' nrow(df)
#' nrow(distinct(df))
#' nrow(distinct(df, x, y))
#'
#' distinct(df, x)
#' distinct(df, y)
#'
#' # Can choose to keep all other variables as well
#' distinct(df, x, .keep_all = TRUE)
#' distinct(df, y, .keep_all = TRUE)
#'
#' # You can also use distinct on computed variables
#' distinct(df, diff = abs(x - y))
#'
#' # The same behaviour applies for grouped data frames
#' # except that the grouping variables are always included
#' df <- tibble(
#'   g = c(1, 1, 2, 2),
#'   x = c(1, 1, 2, 1)
#' ) %>% group_by(g)
#' df %>% distinct()
#' df %>% distinct(x)
#'
#' # Values in list columns are compared by reference, this can lead to
#' # surprising results
#' tibble(a = as.list(c(1, 1, 2))) %>% glimpse() %>% distinct()
#' tibble(a = as.list(1:2)[c(1, 1, 2)]) %>% glimpse() %>% distinct()
distinct <- function(.data, ..., .keep_all = FALSE) {
  UseMethod("distinct")
}
#' @export
distinct.default <- function(.data, ..., .keep_all = FALSE) {
  distinct_(.data, .dots = compat_as_lazy_dots(...), .keep_all = .keep_all)
}
#' @export
#' @rdname se-deprecated
#' @inheritParams distinct
distinct_ <- function(.data, ..., .dots, .keep_all = FALSE) {
  signal_soft_deprecated(paste_line(
    "distinct_() is deprecated. ",
    "Please use distinct() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with distinct() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("distinct_")
}

#' Same basic philosophy as group_by_prepare(): lazy_dots comes in, list of data and
#' vars (character vector) comes out.
#' @rdname group_by_prepare
#' @export
distinct_prepare <- function(.data, vars, group_vars = character(), .keep_all = FALSE) {
  stopifnot(is_quosures(vars), is.character(group_vars))

  # If no input, keep all variables
  if (length(vars) == 0) {
    vars <- list_cols_warning(.data, seq_along(.data))
    return(list(
      data = .data,
      vars = vars,
      keep = seq_along(.data)
    ))
  }

  # If any calls, use mutate to add new columns, then distinct on those
  .data <- add_computed_columns(.data, vars)
  vars <- exprs_auto_name(vars)

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  missing_vars <- setdiff(names(vars), names(.data))

  if (length(missing_vars) > 0) {
    missing_items <- fmt_items(fmt_obj(missing_vars))
    vars <- vars[names(vars) %in% names(.data)]
    if (length(vars) > 0) {
      true_vars <- glue("The following variables will be used:
                        {fmt_items(names(vars))}")
    } else {
      true_vars <- "The operation will return the input unchanged."
    }
    msg <- glue("Trying to compute distinct() for variables not found in the data:
                {missing_items}
                This is an error, but only a warning is raised for compatibility reasons.
                {true_vars}
                ")
    warn(msg)
  }

  new_vars <- unique(c(names(vars), group_vars))

  # Keep the order of the variables
  out_vars <- intersect(new_vars, names(.data))

  if (.keep_all) {
    keep <- seq_along(.data)
  } else {
    keep <- unique(out_vars)
  }

  out_vars <- list_cols_warning(.data, out_vars)
  list(data = .data, vars = out_vars, keep = keep)
}

#' Throw an error if there are tbl columns of type list
#'
#' @noRd
list_cols_warning <- function(df, keep_cols) {
  df_keep <- df[keep_cols]
  lists <- map_lgl(df_keep, is.list)
  if (any(lists)) {
    items <- fmt_items(fmt_obj(names(df_keep)[lists]))
    warn(
      glue("distinct() does not fully support columns of type `list`.
            List elements are compared by reference, see ?distinct for details.
            This affects the following columns:
            {items}")
    )
  }
  keep_cols
}

#' Efficiently count the number of unique values in a set of vector
#'
#' This is a faster and more concise equivalent of `length(unique(x))`
#'
#' @param \dots vectors of values
#' @param na.rm if `TRUE` missing values don't count
#' @examples
#' x <- sample(1:10, 1e5, rep = TRUE)
#' length(unique(x))
#' n_distinct(x)
#' @export
n_distinct <- function(..., na.rm = FALSE) {
  data <- tibble(...)
  if (isTRUE(na.rm)){
    data <- vec_slice(data, !reduce(map(data, vec_equal_na), `|`))
  }
  vec_unique_count(data)
}
