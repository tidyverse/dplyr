#' Subset columns using their names and types
#'
#' Select (and optionally rename) variables in a data frame, using a concise
#' mini-language that makes it easy to refer to variables based on their name
#' (e.g. `a:f` selects all columns from `a` on the left to `f` on the
#' right). You can also use predicate functions like [is.numeric] to select
#' variables based on their properties.
#'
#' @section Useful functions:
#' As well as using existing functions like `:` and `c()`, there are
#' a number of special functions that only work inside `select()`:
#'
#' * [any_of()], [all_of()].
#' * [starts_with()], [ends_with()], [contains()], [matches()].
#' * [num_range()].
#' * [group_cols()], [last_col()].
#' * [everything()].
#'
#' You can also use predicate functions (functions that return a single `TRUE`
#' or `FALSE`) like `is.numeric`, `is.character`, and `is.factor`
#' to select variables of specific types.
#'
#' Selections can be combined using Boolean algebra:
#'
#' * `starts_with("a") & ends_with("x")`: variables with names that start with "a" and end with "x"
#' * `starts_with("a") | starts_with("b")`: variables with names that start with "a" or "b"
#' * `!starts_with("a")`: variables with names that do not start with "a"
#'
#' To remove variables from a selection, use `-`:
#'
#' * `starts_with("a") - ends_width("x")`: variables with names that start with "a" and do not end with "x"
#' * `is.numeric - c(a, b, c)`: numeric variables except, for `a`, `b`, `c`.
#'
#' See [select helpers][tidyselect::select_helpers] for more details and
#' examples.
#'
#' Note that except for `:`, `-` and `c()`, all complex expressions
#' are evaluated outside the data frame context. This is to prevent
#' accidental matching of data frame variables when you refer to
#' variables from the calling environment.
#' @inheritParams arrange
#' @param ... <[`tidy-select`][dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used like they
#'   are positions, so expressions like `x:y` can be used to select a range of
#'   variables.
#' @return
#' An object of the same type as `.data`.
#' * Rows are not affected.
#' * Output columns are a subset of input columns, potentially with a different
#'   order. Columns will be renamed if `new_name = old_name` form is used.
#' * Data frame attributes are preserved.
#' * Groups are maintained.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("select")}.
#' @family single table verbs
#' @export
#' @examples
#' starwars <- as_tibble(starwars) # Convert to simplify appearance of object when printed
#' select(starwars, starts_with("h"))
#' select(starwars, ends_with("color"))
#' select(starwars, !starts_with("h"))
#' select(starwars, starts_with("h") & ends_with("color"))
#' select(starwars, is.numeric)
#'
#' # Optionally, rename individual variables as they are selected, in the format "new_name" = "old_name"
#' select(starwars, character_name = name, character_height = height)
#'
#' # Use num_range() to select variables with numeric suffixes
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' select(df, V4:V6) # Specify variable names explicitly
#' select(df, num_range(prefix = "V", range = 4:6)) # Or, specify the prefix used on a numeric range
#'
#' # Select the existing grouping variables:
#' starwars %>% group_by(gender) %>% select(group_cols())
#'
#' # Using select() semantics in across()
#' starwars %>% summarise(across(height:mass, mean))
#'
#' # Applying tidy eval to select()
#' # See dplyr::tidyeval for more information
#' mycol <- "height"
#' starwars %>% select({{mycol}})
#'
#' # Modifying the order of variables --------------------------
#' # As of dplyr 1.0.0, use relocate(), not select():
#' relocate(iris, Species, .before = 1)
#' relocate(iris, Sepal.Length, .after = last_col())
select <- function(.data, ...) {
  UseMethod("select")
}
#' @export
select.list <- function(.data, ...) {
  abort("`select()` doesn't handle lists.")
}

#' @export
select.data.frame <- function(.data, ...) {
  loc <- tidyselect::eval_select(expr(c(...)), .data)
  loc <- ensure_group_vars(loc, .data, notify = TRUE)

  set_names(.data[loc], names(loc))
}


# Helpers -----------------------------------------------------------------

ensure_group_vars <- function(loc, data, notify = TRUE) {
  group_loc <- match(group_vars(data), names(data))
  missing <- setdiff(group_loc, loc)

  if (length(missing) > 0) {
    vars <- names(data)[missing]
    if (notify) {
      inform(glue(
        "Adding missing grouping variables: ",
        paste0("`", names(data)[missing], "`", collapse = ", ")
      ))
    }
    loc <- c(set_names(missing, vars), loc)
  }

  loc
}
