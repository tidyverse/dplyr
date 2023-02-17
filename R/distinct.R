#' Keep distinct/unique rows
#'
#' Keep only unique/distinct rows from a data frame. This is similar
#' to [unique.data.frame()] but considerably faster.
#'
#' @inheritParams arrange
#' @param ... <[`data-masking`][rlang::args_data_masking]> Optional variables to
#'   use when determining uniqueness. If there are multiple rows for a given
#'   combination of inputs, only the first row will be preserved. If omitted,
#'   will use all variables in the data frame.
#' @param .keep_all If `TRUE`, keep all variables in `.data`.
#'   If a combination of `...` is not distinct, this keeps the
#'   first row of values.
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * Rows are a subset of the input but appear in the same order.
#' * Columns are not modified if `...` is empty or `.keep_all` is `TRUE`.
#'   Otherwise, `distinct()` first calls `mutate()` to create new columns.
#' * Groups are not modified.
#' * Data frame attributes are preserved.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("distinct")}.
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
#' # You can choose to keep all other variables as well
#' distinct(df, x, .keep_all = TRUE)
#' distinct(df, y, .keep_all = TRUE)
#'
#' # You can also use distinct on computed variables
#' distinct(df, diff = abs(x - y))
#'
#' # Use `pick()` to select columns with tidy-select
#' distinct(starwars, pick(contains("color")))
#'
#' # Grouping -------------------------------------------------
#'
#' df <- tibble(
#'   g = c(1, 1, 2, 2, 2),
#'   x = c(1, 1, 2, 1, 2),
#'   y = c(3, 2, 1, 3, 1)
#' )
#' df <- df %>% group_by(g)
#'
#' # With grouped data frames, distinctness is computed within each group
#' df %>% distinct(x)
#'
#' # When `...` are omitted, `distinct()` still computes distinctness using
#' # all variables in the data frame
#' df %>% distinct()
distinct <- function(.data, ..., .keep_all = FALSE) {
  UseMethod("distinct")
}



#' Same basic philosophy as group_by_prepare(): lazy_dots comes in, list of data and
#' vars (character vector) comes out.
#' @rdname group_by_prepare
#' @export
distinct_prepare <- function(.data,
                             vars,
                             group_vars = character(),
                             .keep_all = FALSE,
                             caller_env = caller_env(2),
                             error_call = caller_env()
                             ) {
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
  computed_columns <- add_computed_columns(.data, vars, error_call = error_call)
  .data <- computed_columns$data
  distinct_vars <- computed_columns$added_names

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  missing_vars <- setdiff(distinct_vars, names(.data))
  if (length(missing_vars) > 0) {
    bullets <- c(
      "Must use existing variables.",
      set_names(glue("`{missing_vars}` not found in `.data`."), rep("x", length(missing_vars)))
    )
    abort(bullets, call = error_call)
  }

  # Only keep unique vars
  distinct_vars <- unique(distinct_vars)
  # Missing grouping variables are added to the front
  new_vars <- c(setdiff(group_vars, distinct_vars), distinct_vars)

  if (.keep_all) {
    keep <- seq_along(.data)
  } else {
    keep <- new_vars
  }

  list(data = .data, vars = new_vars, keep = keep)
}

#' @export
distinct.data.frame <- function(.data, ..., .keep_all = FALSE) {
  prep <- distinct_prepare(
    .data,
    vars = enquos(...),
    group_vars = group_vars(.data),
    .keep_all = .keep_all,
    caller_env = caller_env()
  )

  out <- prep$data

  cols <- dplyr_col_select(out, prep$vars)
  loc <- vec_unique_loc(cols)

  out <- dplyr_col_select(out, prep$keep)
  dplyr_row_slice(out, loc)
}
