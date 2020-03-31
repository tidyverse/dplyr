#' Subset distinct/unique rows
#'
#' Select only unique/distinct rows from a data frame. This is similar
#' to [unique.data.frame()] but considerably faster.
#'
#' @inheritParams arrange
#' @param ... <[`data-masking`][dplyr_data_masking]> Optional variables to use
#'   when determining uniqueness. If there are multiple rows for a given
#'   combination of inputs, only the first row will be preserved. If omitted,
#'   will use all variables.
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
#' # use across() to access select()-style semantics
#' distinct(starwars, across(contains("color")))
#'
#' # Grouping -------------------------------------------------
#' # The same behaviour applies for grouped data frames,
#' # except that the grouping variables are always included
#' df <- tibble(
#'   g = c(1, 1, 2, 2),
#'   x = c(1, 1, 2, 1)
#' ) %>% group_by(g)
#' df %>% distinct(x)
#'
distinct <- function(.data, ..., .keep_all = FALSE) {
  UseMethod("distinct")
}



#' Same basic philosophy as group_by_prepare(): lazy_dots comes in, list of data and
#' vars (character vector) comes out.
#' @rdname group_by_prepare
#' @export
distinct_prepare <- function(.data, vars, group_vars = character(), .keep_all = FALSE) {
  abort_if_not(is_quosures(vars), is.character(group_vars))

  # If no input, keep all variables
  if (length(vars) == 0) {
    return(list(
      data = .data,
      vars = seq_along(.data),
      keep = seq_along(.data)
    ))
  }

  # If any calls, use mutate to add new columns, then distinct on those
  computed_columns <- add_computed_columns(.data, vars)
  .data <- computed_columns$data
  distinct_vars <- computed_columns$added_names

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
distinct.data.frame <- function(.data, ..., .keep_all = FALSE) {
  prep <- distinct_prepare(.data,
    vars = enquos(...),
    group_vars = group_vars(.data),
    .keep_all = .keep_all
  )

  # out <- as_tibble(prep$data)
  out <- prep$data
  loc <- vec_unique_loc(as_tibble(out)[prep$vars])


  dplyr_row_slice(out[prep$keep], loc)
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
  columns <- map(enquos(..., .named = TRUE), eval_tidy)
  data <- as_tibble(columns, .name_repair = "minimal")
  if (isTRUE(na.rm)){
    data <- vec_slice(data, !reduce(map(data, vec_equal_na), `|`))
  }
  vec_unique_count(data)
}
