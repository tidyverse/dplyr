#' Create, modify, and delete columns
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `transmute()` creates a new data frame containing only the specified
#' computations. It's superseded because you can perform the same job
#' with `mutate(.keep = "none")`.
#'
#' @inheritParams mutate
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("transmute")}.
#' @returns An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * Columns created or modified through `...` will be returned in the order
#'   specified by `...`.
#' * Unmodified grouping columns will be placed at the front.
#' * The number of rows is not affected.
#' * Columns given the value `NULL` will be removed.
#' * Groups will be recomputed if a grouping variable is mutated.
#' * Data frame attributes are preserved.
#' @keywords internal
#' @export
transmute <- function(.data, ...) {
  # dplyr 1.1.0
  lifecycle::signal_stage("superseded", "transmute()", I("mutate(.keep = 'none')"))

  UseMethod("transmute")
}

#' @export
transmute.data.frame <- function(.data, ...) {
  dots <- check_transmute_args(...)
  dots <- dplyr_quosures(!!!dots)

  # We don't expose `.by` because `transmute()` is superseded
  by <- compute_by(by = NULL, data = .data)

  cols <- mutate_cols(.data, dots, by)

  out <- dplyr_col_modify(.data, cols)

  # Compact out `NULL` columns that got removed.
  # These won't exist in `out`, but we don't want them to look "new".
  # Note that `dplyr_col_modify()` makes it impossible to `NULL` a group column,
  # which we rely on below.
  cols <- compact_null(cols)

  # Retain expression columns in order of their appearance
  cols_expr <- names(cols)

  # Retain untouched group variables up front
  cols_group <- by$names
  cols_group <- setdiff(cols_group, cols_expr)

  cols_retain <- c(cols_group, cols_expr)

  dplyr_col_select(out, cols_retain)
}

# helpers -----------------------------------------------------------------

check_transmute_args <- function(..., .keep, .before, .after, error_call = caller_env()) {
  if (!missing(.keep)) {
    abort("The `.keep` argument is not supported.", call = error_call)
  }
  if (!missing(.before)) {
    abort("The `.before` argument is not supported.", call = error_call)
  }
  if (!missing(.after)) {
    abort("The `.after` argument is not supported.", call = error_call)
  }
  enquos(...)
}
