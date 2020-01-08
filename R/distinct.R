#' Select distinct/unique rows
#'
#' Select only unique/distinct rows from a data frame. This is similar
#' to [unique.data.frame()], but considerably faster.
#'
#' @param .data a tbl
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Optional variables to use when
#'   determining uniqueness. If there are multiple rows for a given combination
#'   of inputs, only the first row will be preserved. If omitted, will use all
#'   variables.
#' @param .keep_all If `TRUE`, keep all variables in `.data`.
#'   If a combination of `...` is not distinct, this keeps the
#'   first row of values.
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
distinct <- function(.data, ..., .keep_all = FALSE) {
  UseMethod("distinct")
}

#' Same basic philosophy as group_by_prepare(): lazy_dots comes in, list of data and
#' vars (character vector) comes out.
#' @rdname group_by_prepare
#' @export
distinct_prepare <- function(.data, vars, group_vars = character(), .keep_all = FALSE) {
  stopifnot(is_quosures(vars), is.character(group_vars))

  # If no input, keep all variables
  if (length(vars) == 0) {
    return(list(
      data = .data,
      vars = seq_along(.data),
      keep = seq_along(.data)
    ))
  }

  # If any calls, use mutate to add new columns, then distinct on those
  c(.data, distinct_vars) %<-% add_computed_columns(.data, vars)

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  missing_vars <- setdiff(distinct_vars, names(.data))
  if (length(missing_vars) > 0) {
    abort(c(
      "distinct() must use existing variables",
      glue("`{missing_vars}` not found in `.data`")
    ))
  }

  # Always include grouping variables preserving input order
  out_vars <- intersect(names(.data), c(distinct_vars, group_vars))

  if (.keep_all) {
    keep <- seq_along(.data)
  } else {
    keep <- out_vars
  }

  list(data = .data, vars = out_vars, keep = keep)
}


#' @export
distinct.grouped_df <- function(.data, ..., .keep_all = FALSE) {
  dist <- distinct_prepare(
    .data,
    vars = enquos(...),
    group_vars = group_vars(.data),
    .keep_all = .keep_all
  )
  grouped_df(
    vec_slice(
      .data[, dist$keep, drop = FALSE],
      vec_unique_loc(.data[, dist$vars, drop = FALSE])
    ),
    groups(.data),
    group_by_drop_default(.data)
  )
}


#' @export
distinct.data.frame <- function(.data, ..., .keep_all = FALSE) {
  dist <- distinct_prepare(.data, enquos(...), .keep_all = .keep_all)
  vec_slice(
    dist$data[, dist$keep, drop = FALSE],
    vec_unique_loc(dist$data[, dist$vars, drop = FALSE])
  )
}

#' @export
# Can't use NextMethod() in R 3.1, r-lib/rlang#486
distinct.tbl_df <- distinct.data.frame


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
