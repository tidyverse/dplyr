#' Select/rename variables by name
#'
#' Select or rename variables in a data frame, using a concise mini-language
#' that makes it easy to refer to variables based on their name (e.g.
#' `a:f` selects all columns between `a` on the left to `f` on the right).
#' You can also use predicate functions like [is.numeric] to select variables
#' based on their properties.
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
#' to select variables with specific types.
#'
#' Selections can be combined using Boolean algebra like:
#'
#' * `starts_with("a") & ends_with("x")`: start with "a" and end with "x"
#' * `starts_with("a") | starts_with("b")`: start with "a" or "b"
#' * `!starts_with("a")`: doesn't start with "a"
#'
#' To remove variables from a selection, use `-`:
#'
#' * `starts_with("a") - ends_width("x")`: start with "a" and doesn't end with "x"
#' * `is.numeric - c(a, b, c)`: numeric variables except for `a`, `b`, `c`.
#'
#' See [select helpers][tidyselect::select_helpers] for more details and
#' examples.
#'
#' Note that except for `:`, `-` and `c()`, all complex expressions
#' are evaluated outside the data frame context. This is to prevent
#' accidental matching of data frame variables when you refer to
#' variables from the calling context.
#'
#' @section Scoped selection and renaming:
#'
#' The three [scoped] variants of `select()` ([select_all()],
#' [select_if()] and [select_at()]) and the three variants of
#' `rename()` ([rename_all()], [rename_if()], [rename_at()]) make it
#' easy to apply a renaming function to a selection of variables.
#'
#' @inheritParams filter
#' @param ... <[`tidy-select`][dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. You can treat variable names like they
#'   are positions, so you can use expressions like `x:y` to select ranges of
#'   variables.
#'
#'   Use named arguments, e.g. `new_name = old_name`, to rename selected
#'   variables.
#' @return
#' An object of the same type as `.data`. The rows will be left as; only
#' the columns (position and/or name) will be changed.
#' @family single table verbs
#' @export
#' @examples
#' iris <- as_tibble(iris) # so it prints a little nicer
#' select(iris, starts_with("Petal"))
#' select(iris, ends_with("Width"))
#' select(iris, !starts_with("Petal"))
#' select(iris, starts_with("Petal") & ends_with("Width"))
#' select(iris, is.numeric)
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- as_tibble(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
#' select(df, V4:V6)
#' select(df, num_range("V", 4:6))
#'
#' # Select the grouping variables:
#' starwars %>% group_by(gender) %>% select(group_cols())
#'
#' # Moving variables around --------------------------
#' # Move `Species` to the front
#' select(iris, Species, everything())
#'
#' # Move `Sepal.Length` to the back: first select everything except
#' # `Sepal.Length`, then select `Sepal.Length`
#' select(iris, !Sepal.Length, Sepal.Length)
#'
#' # Renaming -----------------------------------------
#' # * select() keeps only the variables you specify
#' select(iris, petal_length = Petal.Length)
#'
#' # * rename() keeps all variables
#' rename(iris, petal_length = Petal.Length)
#'
#' # * select() can rename variables in a group
#' select(iris, obs = starts_with('S'))
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

#' @rdname select
#' @export
rename <- function(.data, ...) {
  UseMethod("rename")
}

#' @export
rename.data.frame <- function(.data, ...) {
  loc <- tidyselect::eval_rename(expr(c(...)), .data)
  # eval_rename() only returns changes
  names <- names(.data)
  names[loc] <- names(loc)

  set_names(.data, names)
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
