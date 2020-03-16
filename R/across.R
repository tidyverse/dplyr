#' Apply a function (or a set of functions) to a set of columns
#'
#' @description
#' `across()` makes it easy to apply the same transformation to multiple
#' columns, allowing you to use [select()] semantics inside in [summarise()] and
#' [mutate()]. `across()` supersedes the family of "scoped variants" like
#' `summarise_at()`, `summarise_if()`, and `summarise_all()`.
#'
#' See `vignette("colwise")` for more details.
#'
#' `c_across()` is designed to work with [rowwise()] to make it easy to
#' perform rowwise aggregations; it works like `c()` but uses tidy select
#' semantics so you can easily select multiple variables. See
#' `vignette("rowwise")` for more details.
#'
#' @param .cols <[`tidy-select`][dplyr_tidy_select]> Columns to transform.
#'   Because `across()` is used within functions like `summarise()` and
#'   `mutate()`, you can't select or compute upon grouping variables.
#' @param .fns Functions to apply to each of the selected columns.
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
#' @param ... Additional arguments for the function calls in `.fns`.
#' @param .names A glue specification that describes how to name the output
#'   columns. This can use `{col}` to stand for the selected column name, and
#'   `{fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{col}"` for the single function case and
#'   `"{col}_{fn}"` for the case where a list is used for `.fns`.
#'
#' @returns
#' A tibble with one column for each column in `.cols` and each function in `.fns`.
#' @examples
#' # across() -----------------------------------------------------------------
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
#' # Use the .names argument to control the output names
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean, .names = "mean_{col}"))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd), .names = "{col}.{fn}"))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean, sd), .names = "{col}.fn{fn}"))
#'
#' # c_across() ---------------------------------------------------------------
#' df <- tibble(id = 1:4, w = runif(4), x = runif(4), y = runif(4), z = runif(4))
#' df %>%
#'   rowwise() %>%
#'   mutate(
#'     sum = sum(c_across(w:z)),
#'     sd = sd(c_across(w:z))
#'  )
#' @export
across <- function(.cols = everything(), .fns = NULL, ..., .names = NULL) {
  key <- key_deparse(sys.call())
  setup <- across_setup({{ .cols }}, fns = .fns, names = .names, key = key)

  vars <- setup$vars
  fns <- setup$fns
  names <- setup$names

  mask <- peek_mask()
  data <- mask$current_cols(vars)

  if (is.null(fns)) {
    nrow <- length(mask$current_rows())
    data <- new_tibble(data, nrow = nrow)

    if (is.null(names)) {
      return(data)
    } else {
      return(set_names(data, names))
    }
  }

  n_cols <- length(data)
  n_fns <- length(fns)

  seq_n_cols <- seq_len(n_cols)
  seq_fns <- seq_len(n_fns)

  k <- 1L
  out <- vector("list", n_cols * n_fns)

  # Reset `cur_column()` info on exit
  old_var <- context_peek_bare("column")
  on.exit(context_poke("column", old_var), add = TRUE)

  # Loop in such an order that all functions are applied
  # to a single column before moving on to the next column
  for (i in seq_n_cols) {
    var <- vars[[i]]
    col <- data[[i]]

    context_poke("column", var)

    for (j in seq_fns) {
      fn <- fns[[j]]
      out[[k]] <- fn(col, ...)
      k <- k + 1L
    }
  }

  names(out) <- names

  as_tibble(out)
}

#' @export
#' @rdname across
c_across <- function(.cols = everything()) {
  key <- key_deparse(sys.call())
  vars <- c_across_setup({{ .cols }}, key = key)

  mask <- peek_mask()

  .cols <- mask$current_cols(vars)
  .cols <- unname(.cols)

  vec_c(!!!.cols)
}

# TODO: The usage of a cache in `across_setup()` and `c_across_setup()` is a stopgap solution, and
# this idea should not be used anywhere else. This should be replaced by the
# next version of hybrid evaluation, which should offer a way for any function
# to do any required "set up" work (like the `eval_select()` call) a single
# time per top-level call, rather than once per group.
across_setup <- function(cols, fns, names, key) {
  mask <- peek_mask()

  value <- mask$across_cache_get(key)
  if (!is.null(value)) {
    return(value)
  }

  cols <- enquo(cols)
  across_cols <- mask$across_cols()

  vars <- tidyselect::eval_select(expr(!!cols), across_cols)
  vars <- names(vars)

  if (is.null(fns)) {
    if (!is.null(names)) {
      names <- glue(names, col = vars, fn = "1")
    }

    value <- list(vars = vars, fns = fns, names = names)
    mask$across_cache_add(key, value)
    return(value)
  }

  # apply `.names` smart default
  if (is.function(fns) || is_formula(fns)) {
    names <- names %||% "{col}"
    fns <- list("1" = fns)
  } else {
    names <- names %||% "{col}_{fn}"
  }

  if (!is.list(fns)) {
    abort("`.fns` must be NULL, a function, a formula, or a list of functions/formulas", class = "dplyr_error_across")
  }

  # handle formulas
  fns <- map(fns, as_function)

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

  names <- glue(names,
    col = rep(vars, each = length(fns)),
    fn  = rep(names_fns, length(vars))
  )

  value <- list(
    vars = vars,
    fns = fns,
    names = names
  )
  mask$across_cache_add(key, value)
  value
}

c_across_setup <- function(cols, key) {
  mask <- peek_mask()

  cols <- enquo(cols)

  value <- mask$across_cache_get(key)
  if (!is.null(value)) {
    return(value)
  }

  across_cols <- mask$across_cols()

  vars <- tidyselect::eval_select(expr(!!cols), across_cols)
  value <- names(vars)

  mask$across_cache_add(key, value)

  value
}

key_deparse <- function(key) {
  deparse(key, width.cutoff = 500L, backtick = TRUE, nlines = 1L, control = NULL)
}
