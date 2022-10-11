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
#' @details
#' For `pick()`, the tidyselection provided through `...` is only evaluated once
#' on the _original_ data frame, and the selection is then reused for all
#' groups. This allows `pick()` to enforce two important invariants:
#'
#' - The columns selected by `pick()` are the same across all groups.
#'
#' - The number of rows returned by `pick()` is always equal to the number of
#'   rows in the current group.
#'
#' For `rowwise()` data frames, list-columns are returned as they appear in the
#' original data frame. This ensures that they can be `pick()`-ed alongside
#' other columns.
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

  # We're using `quos()` instead of `enquos()` here for speed because we aren't
  # defusing named arguments. Only the ellipsis is converted to quosures, there
  # are no further arguments.
  quos <- quos(
    ...,
    .named = NULL,
    .ignore_empty = "all",
    .unquote_names = FALSE
  )

  key <- lapply(quos, quo_get_expr)
  key <- hash(key)

  chops <- mask$tidyselect_cache_get(key)

  if (is.null(chops)) {
    if (length(quos) == 0L) {
      abort("`...` can't be empty.")
    }

    # `pick()` is evaluated in a data mask so we need to remove the
    # mask layer from the quosure environments (same as `across()`) (#5460)
    quos <- map(quos, quo_set_env_to_data_mask_top)
    expr <- expr(c(!!!quos))

    # `pick()` is evaluated on the original data frame, i.e. before any
    # mutations are made. This ensures the cache is valid across expressions.
    size <- mask$get_size()
    data <- mask$get_original_data()
    data <- dplyr_new_tibble(data, size = size)

    # Remove grouping variables from the original data, which are never allowed
    # to be selected as variables to `pick()`. This includes variables
    # specified in `rowwise(.data, ...)`.
    names <- mask$get_group_vars()
    if (length(names) > 0L) {
      data <- data[setdiff(names(data), names)]
    }

    sel <- tidyselect::eval_select(
      expr = expr,
      data = data,
      allow_rename = FALSE
    )

    data <- data[sel]

    if (mask$is_grouped_df() || mask$is_rowwise_df()) {
      # Only chop if we have to
      locs <- mask$get_rows()
      chops <- vec_chop(data, indices = locs)
    } else {
      chops <- list(data)
    }

    mask$tidyselect_cache_push(key, chops)
  }

  group <- mask$get_current_group()

  chops[[group]]
}
