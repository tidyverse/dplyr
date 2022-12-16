#' Select a subset of columns
#'
#' @description
#' `pick()` provides a way to easily select a subset of columns from your data
#' using [select()] semantics while inside a
#' ["data-masking"][dplyr_data_masking] function like [mutate()] or
#' [summarise()]. `pick()` returns a data frame containing the selected columns
#' for the current group.
#'
#' `pick()` is complementary to [across()]:
#' - With `pick()`, you typically apply a function to the full data frame.
#' - With `across()`, you typically apply a function to each column.
#'
#' @details
#' Theoretically, `pick()` is intended to be replaceable with an equivalent call
#' to `tibble()`. For example, `pick(a, c)` could be replaced with
#' `tibble(a = a, c = c)`, and `pick(everything())` on a data frame with cols
#' `a`, `b`, and `c` could be replaced with `tibble(a = a, b = b, c = c)`.
#'
#' @param ... <[`tidy-select`][dplyr_tidy_select]>
#'
#'   Columns to pick.
#'
#'   You can't pick grouping columns because they are already automatically
#'   handled by the verb (i.e. [summarise()] or [mutate()]).
#'
#' @returns
#' A tibble containing the selected columns for the current group.
#'
#' @seealso [across()]
#' @export
#' @examples
#' df <- tibble(
#'   x = c(3, 2, 2, 2, 1),
#'   y = c(0, 2, 1, 1, 4),
#'   z1 = c("a", "a", "a", "b", "a"),
#'   z2 = c("c", "d", "d", "a", "c")
#' )
#' df
#'
#' # `pick()` provides a way to select a subset of your columns using
#' # tidyselect. It returns a data frame.
#' df %>% mutate(cols = pick(x, y))
#'
#' # This is useful for functions that take data frames as inputs.
#' # For example, you can compute a joint rank between `x` and `y`.
#' df %>% mutate(rank = dense_rank(pick(x, y)))
#'
#' # `pick()` is also useful as a bridge between data-masking functions (like
#' # `mutate()` or `group_by()`) and functions with tidy-select behavior (like
#' # `select()`). For example, you can use `pick()` to create a wrapper around
#' # `group_by()` that takes a tidy-selection of columns to group on. For more
#' # bridge patterns, see
#' # https://rlang.r-lib.org/reference/topic-data-mask-programming.html#bridge-patterns.
#' my_group_by <- function(data, cols) {
#'   group_by(data, pick({{ cols }}))
#' }
#'
#' df %>% my_group_by(c(x, starts_with("z")))
#'
#' # Or you can use it to dynamically select columns to `count()` by
#' df %>% count(pick(starts_with("z")))
pick <- function(...) {
  # This is the evaluation fallback for `pick()`, which runs:
  # - When users call `pick()` outside of a mutate-like context.
  # - When users wrap `pick()` into their own helper functions, preventing
  #   `pick()` expansion from occurring.

  mask <- peek_mask()

  if (dots_n(...) == 0L) {
    stop_pick_empty()
  }

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

  out <- expand_pick_quo(quo, mask, error_call = error_call)
  out <- new_dplyr_quosure(out, !!!attr(quo, "dplyr:::data"))

  out
}

expand_pick_quo <- function(quo, mask, error_call = caller_env()) {
  env <- quo_get_env(quo)
  expr <- quo_get_expr(quo)

  if (is_missing(expr)) {
    return(quo)
  }

  if (is_quosure(expr)) {
    expr <- expand_pick_quo(expr, mask, error_call = error_call)
  } else if (is_call(expr)) {
    expr <- expand_pick_call(expr, env, mask, error_call = error_call)
  }

  new_quosure(expr, env = env)
}

expand_pick_call <- function(expr, env, mask, error_call = caller_env()) {
  if (is_call(expr, name = "pick", ns = c("", "dplyr"))) {
    expr <- as_pick_selection(expr, error_call)
    out <- eval_pick(expr, env, mask, error_call)
    out <- as_pick_expansion(out)
    return(out)
  }

  if (is_call(expr, name = c("~", "function"))) {
    # Never expand across anonymous function boundaries
    return(expr)
  }

  index <- seq2(2L, length(expr))

  for (i in index) {
    elt <- expr[[i]]

    if (is_missing(elt)) {
      next
    }

    if (is_quosure(elt)) {
      expr[[i]] <- expand_pick_quo(elt, mask, error_call = error_call)
    } else if (is_call(elt)) {
      expr[[i]] <- expand_pick_call(elt, env, mask, error_call = error_call)
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

  out <- tidyselect::eval_select(
    expr = expr,
    env = env,
    data = data,
    error_call = error_call,
    allow_rename = FALSE
  )

  names(out)
}

as_pick_selection <- function(expr, error_call) {
  # Drop `pick()`, get the arguments
  expr <- expr[-1]

  if (is.null(expr)) {
    stop_pick_empty(call = error_call)
  }

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
  # a `reframe()` call that modified columns in an earlier expression like
  # `reframe(df, x = 1, y = pick(x, z))`. This also exactly mimics expansion
  # into `y = tibble(x, z)`.
  size <- vec_size_common(!!!out, .call = error_call)
  out <- vec_recycle_common(!!!out, .size = size, .call = error_call)

  dplyr_new_tibble(out, size = size)
}

stop_pick_empty <- function(call = caller_env()) {
  abort("Must supply at least one input to `pick()`.", call = call)
}
