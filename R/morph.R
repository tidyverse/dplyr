#' Create a new data frame
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `morph()` creates a new data frame by applying functions to columns of an
#' existing data frame. It is very similar to [summarise()], with two big
#' differences:
#'
#' - `morph()` can return an arbitrary number of rows per group, while
#'   `summarise()` reduces each group down to a single row.
#'
#' - `morph()` always returns an ungrouped data frame, while `summarise()` might
#'   return a grouped or rowwise data frame, depending on the scenario.
#'
#' We expect that you'll use `summarise()` much more often than `morph()`, but
#' `morph()` can be particularly helpful when you need to apply a complex
#' function that doesn't return a single summary value.
#'
#' @inheritParams args_by
#' @inheritParams arrange
#'
#' @param ... <[`data-masking`][dplyr_data_masking]>
#'
#'   Name-value pairs of functions. The name will be the name of the variable in
#'   the result. The value can be a vector of any length.
#'
#'   Unnamed data frame values add multiple columns from a single expression.
#'
#' @return
#' If `.data` is a tibble, a tibble. Otherwise, a data.frame.
#'
#' * The rows originate from the underlying grouping keys.
#' * The columns are a combination of the grouping keys and the
#'   expressions that you provide.
#' * The output is always ungrouped.
#' * Data frame attributes are **not** preserved, because `morph()`
#'   fundamentally creates a new data frame.
#'
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("morph")}.
#'
#' @family single table verbs
#' @export
#' @examples
#' table <- c("a", "b", "d", "f")
#'
#' df <- tibble(
#'   g = c(1, 1, 1, 2, 2, 2, 2),
#'   x = c("e", "a", "b", "c", "f", "d", "a")
#' )
#'
#' # `morph()` allows you to apply functions that return
#' # an arbitrary number of rows
#' df %>%
#'   morph(x = intersect(x, table))
#'
#' # Functions are applied per group, and each group can return a
#' # different number of rows.
#' df %>%
#'   morph(x = intersect(x, table), .by = g)
#'
#' # The output is always ungrouped, even when using `group_by()`
#' df %>%
#'   group_by(g) %>%
#'   morph(x = intersect(x, table))
#'
#' # You can add multiple columns at once using a single expression by returning
#' # a data frame.
#' quantile_df <- function(x, probs = c(0.25, 0.5, 0.75)) {
#'   tibble(
#'     val = quantile(x, probs, na.rm = TRUE),
#'     quant = probs
#'   )
#' }
#'
#' x <- c(10, 15, 18, 12)
#' quantile_df(x)
#'
#' starwars %>%
#'   morph(quantile_df(height))
#'
#' starwars %>%
#'   morph(quantile_df(height), .by = homeworld)
#'
#' starwars %>%
#'   morph(
#'     across(c(height, mass), quantile_df, .unpack = TRUE),
#'     .by = homeworld
#'   )
morph <- function(.data, ..., .by = NULL) {
  UseMethod("morph")
}

#' @export
morph.data.frame <- function(.data, ..., .by = NULL) {
  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")

  cols <- summarise_cols(.data, dplyr_quosures(...), by, "morph")
  out <- summarise_build(by, cols)

  if (!is_tibble(.data)) {
    # The `by` group data we build from is always a tibble,
    # so we have to manually downcast as needed
    out <- as.data.frame(out)
  }

  out
}
