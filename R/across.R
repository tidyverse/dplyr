#' Apply a function (or a set of functions) to a set of columns
#'
#' `across()` makes it easy to apply the same transformation to multiple
#' columns, allowing you to use [select()] semantics inside in [summarise()] and
#' [mutate()]. `across()` supersedes the family of "scoped variants" like
#' `summarise_at()`, `summarise_if()`, and `summarise_all()`.
#' See `vignette("colwise")` for more details.
#'
#' @param cols <[`tidy-select`][dplyr_tidy_select]> Columns to transform.
#'   Because `across()` is used within functions like `summarise()` and
#'   `mutate()`, you can't select or compute upon grouping variables.
#' @param fns Functions to apply to each of the selected columns.
#'   Possible values are:
#'
#'   - `NULL`, to returns the columns untransformed.
#'   - A function, e.g. `mean`.
#'   - A purrr-style lambda, e.g. `~ mean(.x, na.rm = TRUE)`
#'   - A list of functions/lambdas, e.g.
#'     `list(mean = mean, n_miss = ~ sum(is.na(.x))`
#'
#'   Within these functions you can use [cur_column()] and [cur_group()]
#'   to access the current column and grouping keys respectively.
#' @param names A glue specification that describes how to name the output
#'   columns. This can use `{col}` to stand for the selected column name, and
#'   `{fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{col}"` for the single function case and
#'   `"{col}_{fn}"` for the case where a list is used for `fns`.
#' @param ... Additional arguments for the function calls in `fns`.
#'
#' @returns
#' A tibble with one column for each column in `cols` and each function in `fns`.
#' @examples
#' # A function
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean))
#' iris %>%
#'   as_tibble() %>%
#'   mutate(across(is.factor, as.character))
#'
#' # A purrr-style formula
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), ~mean(.x, na.rm = TRUE)))
#'
#' # A named list of functions
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd)))
#'
#' # Use the names argument to control the output names
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean, names = "mean_{col}"))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd), names = "{col}.{fn}"))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean, sd), names = "{col}.fn{fn}"))
#'
#' @export
across <- function(cols = everything(), fns = NULL, names = NULL, ...) {
  vars <- across_select({{ cols }})

  mask <- peek_mask()
  data <- mask$current_cols(vars)

  if (is.null(fns)) {
    nrow <- length(mask$current_rows())
    data <- new_tibble(data, nrow = nrow)

    if (is.null(names)) {
      return(data)
    } else {
      return(set_names(data, glue(names, col = names(data), fn = "1")))
    }
  }

  # apply `names` smart default
  if (is.function(fns) || is_formula(fns)) {
    names <- names %||% "{col}"
    fns <- list("1" = fns)
  } else {
    names <- names %||% "{col}_{fn}"
  }

  if (!is.list(fns)) {
    abort("`fns` must be NULL, a function, a formula, or a list of functions/formulas", class = "dplyr_error_across")
  }

  # make sure fns has names, use number to replace unnamed
  if (is.null(names(fns))) {
    names_fns <- seq_along(fns)
  } else {
    names_fns <- names(fns)
    empties <- which(names_fns == "")
    if (length(empties)) {
      names_fns[empties] <- empties
    }
  }

  # handle formulas
  fns <- map(fns, as_function)

  # main loop
  cols <- pmap(
    expand.grid(i = seq_along(data), fn = fns),
    function(i, fn) {
      local_column(vars[i])
      fn(data[[i]], ...)
    }
  )
  names(cols) <- glue(names,
    col = rep(vars, each = length(fns)),
    fn  = rep(names_fns, length(data))
  )
  as_tibble(cols)
}

# TODO: The usage of a cache in `across_select()` is a stopgap solution, and
# this idea should not be used anywhere else. This should be replaced by the
# next version of hybrid evaluation, which should offer a way for any function
# to do any required "set up" work (like the `eval_select()` call) a single
# time per top-level call, rather than once per group.
across_select <- function(cols) {
  mask <- peek_mask()

  cols <- enquo(cols)

  key <- quo_get_expr(cols)
  key <- expr_text(key, width = 500L)

  cache <- mask$across_cache_get()
  value <- cache[[key]]

  if (!is.null(value)) {
    return(value)
  }

  across_cols <- mask$across_cols()

  vars <- tidyselect::eval_select(expr(!!cols), across_cols)
  value <- names(vars)

  mask$across_cache_add(key, value)

  value
}
