#' Select a subset of columns
#'
#' @description
#' `pick()` provides a way to easily select a subset of columns from your data
#' using `select()` semantics while inside a "data-masking" function like
#' `mutate()` or `summarise()`.
#'
#' `pick()`'s main purpose is as a tool for easy column selection. Typically,
#' you compute on the data frame returned by `pick()` as a whole, rather than on
#' each column individually. To apply a function across multiple columns, see
#' [across()].
#'
#' @param ... <[`tidy-select`][dplyr_tidy_select]>
#'
#'   Columns to pick. Because `pick()` is used within functions like
#'   `mutate()` and `summarise()`, you can't pick grouping variables.
#'
#' @return
#' A tibble containing the selected columns.
#'
#' @seealso [across()]
#' @export
#' @examples
#' df <- tibble(
#'   x = c(3, 2, 2, 2, 1),
#'   y = c(0, 2, 1, 1, 4),
#'   z = c("a", "a", "a", "b", "a")
#' )
#' df
#'
#' # `pick()` provides a way to select a subset of your columns using
#' # tidyselect. It returns a data frame.
#' df %>% mutate(cols = pick(y, z))
#'
#' # This is useful for functions that take data frames as inputs.
#' # For example, you can compute a joint rank between `x` and `y`.
#' df %>% mutate(rank = dense_rank(pick(x, y)))
#'
#' # Or compute a consecutive id column based off `x` and `z` that increments
#' # any time a value changes in either column
#' df %>% mutate(id = consecutive_id(pick(x, z))) %>% select(id, x, z)
#'
#' # `pick()` is also useful as a way to augment "data-masking" functions with
#' # `select()` (i.e. tidyselect) semantics. For example, you can use `pick()`
#' # to create a wrapper around `group_by()` that takes a tidyselection of
#' # columns to group on.
#' my_group_by <- function(data, cols) {
#'   group_by(data, pick({{ cols }}))
#' }
#'
#' my_group_by(df, c(x, z))
pick <- function(...) {
  mask <- peek_mask()

  # `...` are evaluated on the current state of "full" unchopped columns,
  # not just on the current group chop, to ensure that per-group tidyselect
  # results are consistent
  data <- mask$get_current_data(groups = FALSE)

  if (dots_n(...) == 0L) {
    abort("`...` can't be empty.")
  }

  # `pick()` is evaluated in a data mask so we need to remove the
  # mask layer from the quosure environments (same as `across()`) (#5460)
  quos <- enquos(..., .named = NULL)
  quos <- map(quos, quo_set_env_to_data_mask_top)
  expr <- expr(c(!!!quos))

  sel <- tidyselect::eval_select(
    expr = expr,
    data = data,
    allow_rename = FALSE
  )
  sel <- names(sel)

  mask$pick(sel)
}
