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
#' `pick()` can be thought of as being replaceable with the equivalent call to
#' `tibble()`. For example, `pick(a, c)` could be replaced with
#' `tibble(a = a, c = c)`, and `pick(everything())` on a data frame with cols
#' `a`, `b`, and `c` could be replaced with `tibble(a = a, b = b, c = c)`.
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
  # This is the evaluation fallback for `pick()`, which runs:
  # - When users call `pick()` outside of a mutate-like context.
  # - When users wrap `pick()` into their own helper functions, preventing
  #   `pick()` expansion from occurring.

  mask <- peek_mask()

  # Evaluates `pick()` on current columns.
  # Mimicking expansion as much as possible, which should match the idea of
  # replacing the `pick()` call directly with `tibble()`, like:
  # pick(a, b, starts_with("foo")) -> tibble(a = a, b = b, foo1 = foo1)
  non_group_vars <- mask$current_non_group_vars()
  data <- mask$current_cols(non_group_vars)

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

  data <- data[sel]
  data <- dplyr_pick_tibble(!!!data)

  data
}

# ------------------------------------------------------------------------------

expand_pick <- function(quo, mask) {
  error_call <- call("pick")

  env <- quo_get_env(quo)
  expr <- quo_get_expr(quo)

  if (!is_missing(expr) && !is_quosure(expr) && is_call(expr)) {
    expr <- expand_pick_impl(expr, env, mask, error_call)
  }

  out <- new_quosure(expr, env = env)
  out <- new_dplyr_quosure(out, !!!attr(quo, "dplyr:::data"))

  out
}

expand_pick_impl <- function(expr, env, mask, error_call = caller_env()) {
  if (is_call(expr, name = "pick", ns = c("", "dplyr"))) {
    expr <- as_pick_selection(expr, error_call)
    out <- eval_pick(expr, env, mask, error_call)
    out <- as_pick_expansion(out)
    return(out)
  }

  index <- seq2(2L, length(expr))

  for (i in index) {
    elt <- expr[[i]]

    if (!is_missing(elt) && !is_quosure(elt) && is_call(elt)) {
      expr[[i]] <- expand_pick_impl(elt, env, mask, error_call = error_call)
    }
  }

  expr
}

eval_pick <- function(expr, env, mask, error_call = caller_env()) {
  # Evaluates `pick()` on the full version of the "current" columns.
  # Remove grouping variables, which are never allowed to be selected as
  # variables to `pick()`. This includes variables specified in
  # `rowwise(.data, ...)`.
  data <- mask$get_current_data(groups = FALSE)

  out <- with_pick_tidyselect_errors(tidyselect::eval_select(
    expr = expr,
    env = env,
    data = data,
    error_call = error_call,
    allow_rename = FALSE
  ))

  names(out)
}

with_pick_tidyselect_errors <- function(expr) {
  try_fetch(
    expr,
    error = function(cnd) {
      # Subclassed so we can skip computing group context info for them
      class(cnd) <- c("dplyr:::error_pick_tidyselect", class(cnd))
      cnd_signal(cnd)
    }
  )
}

as_pick_selection <- function(expr, error_call) {
  # Drop `pick()`, get the arguments
  expr <- expr[-1]

  # Turn arguments into list of expressions
  expr <- as.list(expr)

  # Inline into `c()` call for tidy-selection
  expr <- expr(c(!!!expr))

  expr
}

as_pick_expansion <- function(names) {
  out <- set_names(syms(names), names)
  expr(asNamespace("dplyr")$dplyr_pick_tibble(!!!out))
}

dplyr_pick_tibble <- function(...) {
  error_call <- call("pick")

  out <- list2(...)

  # Allow recycling between selected columns, in case it is called from
  # a `summarise()` call that modified columns in an earlier expression like
  # `summarise(df, x = 1, y = pick(x, z))`. This also exactly mimics expansion
  # into `y = tibble(x, z)`.
  size <- vec_size_common(!!!out, .call = error_call)
  out <- vec_recycle_common(!!!out, .size = size, .call = error_call)

  dplyr_new_tibble(out, size = size)
}
