#' Perform an operation with temporary groups
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' This is an experimental new function that allows you to modify the grouping
#' variables for a single operation.
#'
#' @param .data A data frame
#' @param .groups <[`tidy-select`][dplyr_tidy_select]> One or more variables
#'   to group by. Unlike [group_by()], you can only group by existing variables,
#'   and you can use tidy-select syntax like `c(x, y, z)` to select multiple
#'   variables.
#'
#'   Use `NULL` to temporarily **un**group.
#' @param .f Function to apply to regrouped data.
#'   Supports purrr-style `~` syntax
#' @param ... Additional arguments passed on to `...`.
#' @export
#' @examples
#' df <- tibble(g = c(1, 1, 2, 2, 3), x = runif(5))
#' df %>%
#'   with_groups(g, mutate, x_mean = mean(x))
#' df %>%
#'   with_groups(g, ~ mutate(.x, x1 = first(x)))
#'
#' df %>%
#'   group_by(g) %>%
#'   with_groups(NULL, mutate, x_mean = mean(x))
#'
#' # NB: grouping can't be restored if you remove the grouping variables
#' df %>%
#'   group_by(g) %>%
#'   with_groups(NULL, mutate, g = NULL)
with_groups <- function(.data, .groups, .f, ...) {
  loc <- tidyselect::eval_select(enquo(.groups), data = tbl_ptype(.data))
  val <- syms(names(.data)[loc])
  out <- group_by(.data, !!!val)

  .f <- as_function(.f)
  out <- .f(out, ...)
  dplyr_reconstruct(out, .data)
}
