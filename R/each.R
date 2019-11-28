#' key data frame for the current group
#'
#' @return A data frame, with a single row, and one column per grouping variable
#'
#' @export
current_key <- function() {
  peek_mask()$current_key()
}

set_current_column <- function(name) {
  context_env[["..current_column_name"]] <- name
}

poke_current_column <- function(name) {
  old <- context_env[["..current_column_name"]]
  set_current_column(name)
  old
}

#' @rdname across
#' @export
current_column <- function() {
  context_env[["..current_column_name"]] %||% abort("No current column name registered, current_column() only makes sense inside across()")
}

#' Apply a function or a set of functions to a set of columns
#'
#' Creates a data frame by applying a set of functions to a tidy
#' selection of columns in the current slice
#'
#' @param select tidy selection of columns
#' @param fns Functions to apply to each of the selected columns. Possible
#'   values are:
#'
#'   - A single function or a single quosure style lambda, e.g. `~ mean(.x, na.rm = TRUE)`
#'   - A named list of functions and/or lambdas
#'
#'  @return A tibble
#'
#'  @details
#'
#'  When a single function is given, it is applied to each of the selected columns
#'  and the output columns are named after the selected input columns.
#'
#'  When a named list of functions is given, the result is made of as many columns
#'  as the number of functions, each of these output columns is a pack, i.e.
#'  a data frame column made from the result of the function applied across the
#'  selected columns.
#'
#'  `current_column()` gives the name of the column currently being processed.
#'
#' @examples
#'
#' # A single function
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean))
#'
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), ~mean(.x, na.rm = TRUE)))
#'
#' # a named list of functions
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd)))
#'
#' @importFrom tidyselect vars_select
#' @export
across <- function(select, fns = identity) {
  mask <- peek_mask()
  vars <- vars_select(peek_vars(), {{select}})
  data <- mask$pick(vars)

  single_function <- is.function(fns) || is_formula(fns)

  if (single_function) {
    fns <- as_function(fns)
  } else {
    if (is.null(names(fns))) {
      abort("funs should be a single function, a single formula, or a named list of functions or formulas")
    }
    fns <- map(fns, as_function)
  }

  f <- function(df) {
    if (single_function) {
      as_tibble(imap(df, function(.x, .y) {
        old <- poke_current_column(.y)
        on.exit(set_current_column(old))
        fns(.x)
      }))
    } else {
      results <- map(fns, function(f) {
        as_tibble(imap(df, function(.x, .y) {
          old <- poke_current_column(.y)
          on.exit(set_current_column(old))
          f(.x)
        }))
      })
      tibble(!!!results)
    }
  }

  f(data)
}
