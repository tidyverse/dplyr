#' Select/rename variables by name
#'
#' Choose or rename variables from a tbl.
#' `select()` keeps only the variables you mention; `rename()`
#' keeps all variables.
#'
#' These functions work by column index, not value; thus, an expression
#' like `select(data.frame(x = 1:5, y = 10), z = x+1)` does not create a variable
#' with values `2:6`. (In the current implementation, the expression `z = x+1`
#' wouldn't do anything useful.)  To calculate using column values, see
#' [mutate()]/[transmute()].
#'
#' @section Useful functions:
#' As well as using existing functions like `:` and `c()`, there are
#' a number of special functions that only work inside `select()`:
#'
#' * [starts_with()], [ends_with()], [contains()]
#' * [matches()]
#' * [num_range()]
#' * [one_of()]
#' * [everything()]
#' * [group_cols()]
#'
#' To drop variables, use `-`.
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
#' @inheritSection filter Tidy data
#' @param ... <[`tidy-select`][dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. You can treat variable names like they
#'   are positions, so you can use expressions like `x:y` to select ranges of
#'   variables.
#'
#'   Positive values select variables; negative values drop variables.
#'   If the first expression is negative, `select()` will automatically
#'   start with all variables.
#'
#'   Use named arguments, e.g. `new_name = old_name`, to rename selected variables.
#'   See [select helpers][tidyselect::select_helpers] for more details and
#'   examples about tidyselect helpers such as `starts_with()`, `everything()`, ...
#' @return An object of the same class as `.data`.
#' @family single table verbs
#' @export
#' @examples
#' iris <- as_tibble(iris) # so it prints a little nicer
#' select(iris, starts_with("Petal"))
#' select(iris, ends_with("Width"))
#'
#' # Move Species variable to the front
#' select(iris, Species, everything())
#'
#' # Move Sepal.Length variable to back
#' # first select all variables except Sepal.Length, then re select Sepal.Length
#' select(iris, -Sepal.Length, Sepal.Length)
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- as_tibble(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
#' select(df, V4:V6)
#' select(df, num_range("V", 4:6))
#'
#' # Drop variables with -
#' select(iris, -starts_with("Petal"))
#'
#' # Select the grouping variables:
#' starwars %>% group_by(gender) %>% select(group_cols())
#'
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

#' @rdname select
#' @export
rename <- function(.data, ...) {
  UseMethod("rename")
}

#' @export
select.data.frame <- function(.data, ...) {
  idx <- tidyselect::eval_select(expr(c(...)), .data)
  set_names(.data[, idx, drop = FALSE], names(idx))
}

#' @export
select.grouped_df <- function(.data, ...) {
  idx <- tidyselect::eval_select(expr(c(...)), .data)
  idx <- ensure_group_vars(idx, .data, notify = TRUE)
  group_idx <- group_idx(.data, idx)

  data <- as.data.frame(.data)
  groups <- group_data(.data)
  group_idx <- c(group_idx, .rows = ncol(groups))

  data <- set_names(data[, idx, drop = FALSE], names(idx))
  groups <- set_names(groups[, group_idx, drop = FALSE], names(group_idx))

  new_grouped_df(data, groups)
}

#' @export
rename.data.frame <- function(.data, ...) {
  idx <- tidyselect::eval_rename(expr(c(...)), .data)
  names(.data)[idx] <- names(idx)

  .data
}

#' @export
rename.grouped_df <- function(.data, ...) {
  idx <- tidyselect::eval_rename(expr(c(...)), .data)
  group_idx <- group_idx(.data, idx)

  data <- as.data.frame(.data)
  groups <- group_data(.data)

  names(data)[idx] <- names(idx)
  names(groups)[group_idx] <- names(group_idx)

  new_grouped_df(data, groups)
}

# Helpers -----------------------------------------------------------------

group_idx <- function(data, idx, force_group = FALSE) {
  vars <- set_names(names(data)[idx], names(idx))
  group_vars <- vars[vars %in% group_vars(data)]
  set_names(match(group_vars, group_vars(data)), names(group_vars))
}

ensure_group_vars <- function(idx, data, notify = TRUE) {
  group_idx <- match(group_vars(data), names(data))
  missing <- setdiff(group_idx, idx)

  if (length(missing) > 0) {
    vars <- names(data)[missing]
    if (notify) {
      inform(glue(
        "Adding missing grouping variables: ",
        paste0("`", names(data)[missing], "`", collapse = ", ")
      ))
    }
    idx <- c(set_names(missing, vars), idx)
  }

  idx
}
