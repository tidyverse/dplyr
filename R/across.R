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
#'   `mutate()`, you can't grouping variables.
#' @param fns Functions to apply to each of the selected columns.
#'   Possible values are:
#'
#'   - `NULL`, to returns the columns untransformed.
#'   - A function, e.g. `mean`.
#'   - A purrr-style lambda, e.g. `~ mean(.x, na.rm = TRUE)`
#'   - A named list of functions/lambdas, e.g.
#'     `list(mean = mean, n_miss = ~ sum(is.na(.x))`
#'
#'   Within these functions you can use [cur_column()] and [cur_group()]
#'   to access the current column and grouping keys respectively.
#' @returns A tibble.
#'
#'   When `fns` is a single function, it will have one column for each
#'   column in `cols`
#'
#'   When `fns` is a named list, it will have one column for each element
#'   of `fns`. Each column will be a df-column that contains one column
#'   for each column in `cols`.
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
#' @export
across <- function(cols = everything(), fns = NULL) {
  mask <- peek_mask()
  data <- mask$full_data()

  vars <- tidyselect::eval_select(
    expr({{ cols }}),
    data[, setdiff(names(data), group_vars(data))]
  )
  data <- mask$pick(names(vars))

  if (is.null(fns)) {
    data
  } else if (is.function(fns) || is_formula(fns)) {
    fns <- as_function(fns)

    as_tibble(imap(data, function(.x, .y) {
      local_column(.y)
      fns(.x)
    }))
  } else if (is.list(fns) && is_named(fns)) {
    fns <- map(fns, as_function)

    as_tibble(map(fns, function(f) {
      as_tibble(imap(data, function(.x, .y) {
        local_column(.y)
        f(.x)
      }))
    }))
  } else {
    abort("`fns` must be NULL, a function, a formula, or a named list of functions/formulas")
  }
}
