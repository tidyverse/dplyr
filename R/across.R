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
#' @param names How to name the result columns.
#' @returns A tibble.
#'
#'   When `fns` is a single function, it will have one column for each
#'   column in `cols`.
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
#'
#' # using custom names
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
across <- function(cols = everything(), fns = NULL, names = NULL) {
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
    cols <- imap(data, function(.x, .y) {
      local_column(.y)
      fns(.x)
    })
    if (!is.null(names)) {
      names(cols) <- glue(names, col = names(data), fn = abort("{fn} cannot be used in the single function case"))
    }
    as_tibble(cols)
  } else if (is.list(fns)) {
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
    fns <- map(fns, as_function)

    if (is.null(names)) {
      names <- "{col}_{fn}"
    }

    cols <- list()
    names_cols <- names(data)
    for (i in seq_along(data)) {
      data_i <- data[[i]]
      name_i <- names_cols[i]
      res <- map(fns, function(f) {
        local_column(name_i)
        f(data_i)
      })
      names(res) <- glue(names, col = names_cols[i], fn = names_fns)
      cols <- append(cols, res)
    }
    as_tibble(cols)
  } else {
    abort("`fns` must be NULL, a function, a formula, or a list of functions/formulas")
  }
}
