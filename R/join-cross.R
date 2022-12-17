#' Cross join
#'
#' @description
#' Cross joins match each row in `x` to every row in `y`, resulting in a data
#' frame with `nrow(x) * nrow(y)` rows.
#'
#' Since cross joins result in all possible matches between `x` and `y`, they
#' technically serve as the basis for all [mutating joins][mutate-joins], which
#' can generally be thought of as cross joins followed by a filter. In practice,
#' a more specialized procedure is used for better performance.
#'
#' @inheritParams left_join
#'
#' @returns
#' An object of the same type as `x` (including the same groups). The output has
#' the following properties:
#'
#' - There are `nrow(x) * nrow(y)` rows returned.
#'
#' - Output columns include all columns from both `x` and `y`. Column name
#'   collisions are resolved using `suffix`.
#'
#' - The order of the rows and columns of `x` is preserved as much as possible.
#'
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("cross_join")}.
#'
#' @family joins
#' @export
#' @examples
#' # Cross joins match each row in `x` to every row in `y`.
#' # Data within the columns is not used in the matching process.
#' cross_join(band_instruments, band_members)
#'
#' # Control the suffix added to variables duplicated in
#' # `x` and `y` with `suffix`.
#' cross_join(band_instruments, band_members, suffix = c("", "_y"))
cross_join <- function(x,
                       y,
                       ...,
                       copy = FALSE,
                       suffix = c(".x", ".y")) {
  UseMethod("cross_join")
}

#' @export
cross_join.data.frame <- function(x,
                                  y,
                                  ...,
                                  copy = FALSE,
                                  suffix = c(".x", ".y")) {
  check_dots_empty0(...)

  y <- auto_copy(x, y, copy = copy)

  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  # Empty join by with no keys
  by <- new_join_by()

  # Particular value isn't too important, as there are no keys to keep/drop
  keep <- FALSE

  vars <- join_cols(
    x_names = x_names,
    y_names = y_names,
    by = by,
    suffix = suffix,
    keep = keep
  )

  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")

  x_size <- vec_size(x_in)
  y_size <- vec_size(y_in)

  x_out <- set_names(x_in, names(vars$x$out))
  y_out <- set_names(y_in, names(vars$y$out))

  x_out <- vec_rep_each(x_out, times = y_size)
  y_out <- vec_rep(y_out, times = x_size)

  x_out[names(y_out)] <- y_out

  dplyr_reconstruct(x_out, x)
}
