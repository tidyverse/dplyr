#' Set operations
#'
#' @description
#' Perform set operations using the rows of a data frame.
#'
#' * `intersect(x, y)` finds all rows in both `x` and `y`.
#' * `union(x, y)` finds all rows in either `x` or `y`, excluding duplicates.
#' * `union_all(x, y)` finds all rows in either `x` or `y`, including duplicates.
#' * `setdiff(x, y)` finds all rows in `x` that aren't in `y`.
#' * `symdiff(x, y)` computes the symmetric difference, i.e. all rows in
#'    `x` that aren't in `y` and all rows in `y` that aren't in `x`.
#' * `setequal(x, y)` returns `TRUE` if `x` and `y` contain the same rows
#'   (ignoring order).
#'
#' Note that `intersect()`, `union()`, `setdiff()`, and `symdiff()` remove
#' duplicates in `x` and `y`.
#'
#' # Base functions
#' `intersect()`, `union()`, `setdiff()`, and `setequal()` override the base
#' functions of the same name in order to make them generic. The existing
#' behaviour for vectors is preserved by providing default methods that call
#' the base functions.
#'
#' @param x,y Pair of compatible data frames. A pair of data frames is
#'   compatible if they have the same column names (possibly in different
#'   orders) and compatible types.
#' @inheritParams rlang::args_dots_empty
#' @name setops
#' @examples
#' df1 <- tibble(x = 1:3)
#' df2 <- tibble(x = 3:5)
#'
#' intersect(df1, df2)
#' union(df1, df2)
#' union_all(df1, df2)
#' setdiff(df1, df2)
#' setdiff(df2, df1)
#' symdiff(df1, df2)
#'
#' setequal(df1, df2)
#' setequal(df1, df1[3:1, ])
#'
#' # Note that the following functions remove pre-existing duplicates:
#' df1 <- tibble(x = c(1:3, 3, 3))
#' df2 <- tibble(x = c(3:5, 5))
#'
#' intersect(df1, df2)
#' union(df1, df2)
#' setdiff(df1, df2)
#' symdiff(df1, df2)
NULL

#' @name setops
#' @aliases intersect
#' @usage intersect(x, y, ...)
#' @importFrom generics intersect
#' @export intersect
NULL

#' @name setops
#' @aliases union
#' @usage union(x, y, ...)
#' @importFrom generics union
#' @export union
NULL

#' @rdname setops
#' @export
union_all <- function(x, y, ...) UseMethod("union_all")
#' @export
union_all.default <- function(x, y, ...) {
  check_dots_empty()
  vec_c(x, y)
}

#' @name setops
#' @aliases setdiff
#' @usage setdiff(x, y, ...)
#' @importFrom generics setdiff
#' @export setdiff
NULL

#' @name setops
#' @aliases setequal
#' @usage setequal(x, y, ...)
#' @importFrom generics setequal
#' @export setequal
NULL

#' @rdname setops
#' @export
symdiff <- function(x, y, ...) {
  UseMethod("symdiff")
}
#' @export
symdiff.default <- function(x, y, ...) {
  check_dots_empty()
  # Default is defined in terms of base R methods
  setdiff(union(x, y), intersect(x, y))
}

#' @export
intersect.data.frame <- function(x, y, ...) {
  check_dots_empty()
  check_compatible(x, y)

  out <- vec_set_intersect(x, y, error_call = current_env())

  dplyr_reconstruct(out, x)
}

#' @export
union.data.frame <- function(x, y, ...) {
  check_dots_empty()
  check_compatible(x, y)

  out <- vec_set_union(x, y, error_call = current_env())

  dplyr_reconstruct(out, x)
}

#' @export
union_all.data.frame <- function(x, y, ...) {
  check_dots_empty()
  check_compatible(x, y)

  out <- vec_rbind(x, y)
  dplyr_reconstruct(out, x)
}

#' @export
setdiff.data.frame <- function(x, y, ...) {
  check_dots_empty()
  check_compatible(x, y)

  out <- vec_set_difference(x, y, error_call = current_env())

  dplyr_reconstruct(out, x)
}

#' @export
setequal.data.frame <- function(x, y, ...) {
  check_dots_empty()
  check_compatible(x, y)

  cast <- vec_cast_common(x = x, y = y)
  all(vec_in(cast$x, cast$y)) && all(vec_in(cast$y, cast$x))
}

#' @export
symdiff.data.frame <- function(x, y, ...) {
  check_dots_empty()
  check_compatible(x, y)

  out <- vec_set_symmetric_difference(x, y, error_call = current_env())

  dplyr_reconstruct(out, x)
}

# Helpers -----------------------------------------------------------------

is_compatible <- function(x, y, ignore_col_order = TRUE, convert = TRUE) {
  if (!is.data.frame(y)) {
    return("`y` must be a data frame.")
  }

  nc <- df_n_col(x)
  if (nc != df_n_col(y)) {
    return(
      c(x = glue("Different number of columns: {nc} vs {df_n_col(y)}."))
    )
  }

  names_x <- names(x)
  names_y <- names(y)

  names_y_not_in_x <- setdiff(names_y, names_x)
  names_x_not_in_y <- setdiff(names_x, names_y)

  if (length(names_y_not_in_x) == 0L && length(names_x_not_in_y) == 0L) {
    # check if same order
    if (!isTRUE(ignore_col_order)) {
      if (!identical(names_x, names_y)) {
        return(c(x = "Same column names, but different order."))
      }
    }
  } else {
    # names are not the same, explain why

    msg <- c()
    if (length(names_y_not_in_x)) {
      wrong <- glue_collapse(glue('`{names_y_not_in_x}`'), sep = ", ")
      msg <- c(
        msg,
        x = glue("Cols in `y` but not `x`: {wrong}.")
      )
    }
    if (length(names_x_not_in_y)) {
      wrong <- glue_collapse(glue('`{names_x_not_in_y}`'), sep = ", ")
      msg <- c(
        msg,
        x = glue("Cols in `x` but not `y`: {wrong}.")
      )
    }
    return(msg)
  }

  msg <- c()
  for (name in names_x) {
    x_i <- x[[name]]
    y_i <- y[[name]]

    if (convert) {
      tryCatch(
        vec_ptype2(x_i, y_i),
        error = function(e) {
          msg <<- c(
            msg,
            x = glue(
              "Incompatible types for column `{name}`: {vec_ptype_full(x_i)} vs {vec_ptype_full(y_i)}."
            )
          )
        }
      )
    } else {
      if (!identical(vec_ptype(x_i), vec_ptype(y_i))) {
        msg <- c(
          msg,
          x = glue(
            "Different types for column `{name}`: {vec_ptype_full(x_i)} vs {vec_ptype_full(y_i)}."
          )
        )
      }
    }
  }
  if (length(msg)) {
    return(msg)
  }

  TRUE
}

check_compatible <- function(
  x,
  y,
  ignore_col_order = TRUE,
  convert = TRUE,
  error_call = caller_env()
) {
  compat <- is_compatible(
    x,
    y,
    ignore_col_order = ignore_col_order,
    convert = convert
  )
  if (isTRUE(compat)) {
    return()
  }

  abort(c("`x` and `y` are not compatible.", compat), call = error_call)
}
