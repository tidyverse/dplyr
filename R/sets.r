#' Set operations
#'
#' @description
#' Perform set operations using the rows of a data frame.
#'
#' * `intersect(x, y)` finds all rows in both `x` and `y`.
#' * `union(x, y)` finds all rows in either `x` or `y`, excluding duplicates.
#' * `union_all(x, y)` finds all rows in either `x` or `y`, including duplicates.
#' * `setdiff(x, y)` finds all rows in `x` that aren't in `y`.
#' * `setequal(x, y)` returns `TRUE` if `x` and `y` contain the same rows
#'   (ignoring order).
#'
#' Note that `intersect()`, `union()` and `setdiff()` remove duplicates
#' in `x` and `y`.
#'
#' # Base functions
#' `intersect()`, `union()`, `setdiff()`, and `setequal()` override the base
#' functions of the same name in order to make them generic. The existing
#' behaviour for vectors is preserved by providing default methods that call
#' the base functions.
#'
#' @param x,y Pair of data frames.
#' @inheritParams rlang::args_dots_empty
#' @name setops
#' @examples
#' mtcars$model <- rownames(mtcars)
#' first <- mtcars[1:20, ]
#' second <- mtcars[10:32, ]
#'
#' intersect(first, second)
#' union(first, second)
#' setdiff(first, second)
#' setdiff(second, first)
#'
#' union_all(first, second)
#' setequal(mtcars, mtcars[32:1, ])
#'
#' # Note the following 3 functions also remove pre-existing duplicates in `x` or `y`:
#' a <- data.frame(x = c(1:3, 3, 3))
#' b <- data.frame(x = c(3:5, 5))
#'
#' intersect(a, b)
#' union(a, b)
#' setdiff(a, b)
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
union_all.default <- function (x, y, ...) {
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

#' @export
intersect.data.frame <- function(x, y, ...) {
  check_dots_empty()
  check_compatible(x, y)

  cast <- vec_cast_common(x = x, y = y)
  out <- vec_unique(vec_slice(cast$x, vec_in(cast$x, cast$y)))
  dplyr_reconstruct(out, x)
}

#' @export
union.data.frame <- function(x, y, ...) {
  check_dots_empty()
  check_compatible(x, y)

  out <- vec_unique(vec_rbind(x, y))
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

  cast <- vec_cast_common(x = x, y = y)
  out <- vec_unique(vec_slice(cast$x, !vec_in(cast$x, cast$y)))
  dplyr_reconstruct(out, x)
}

#' @export
setequal.data.frame <- function(x, y, ...) {
  check_dots_empty()
  if (!is.data.frame(y)) {
    abort("`y` must be a data frame.")
  }
  if (!isTRUE(is_compatible(x, y))) {
    return(FALSE)
  }

  cast <- vec_cast_common(x = x, y = y)
  all(vec_in(cast$x, cast$y)) && all(vec_in(cast$y, cast$x))
}


# Helpers -----------------------------------------------------------------

is_compatible <- function(x, y, ignore_col_order = TRUE, convert = TRUE) {
  if (!is.data.frame(y)) {
    return("`y` must be a data frame.")
  }

  nc <- ncol(x)
  if (nc != ncol(y)) {
    return(
      c(x = glue("Different number of columns: {nc} vs {ncol(y)}."))
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
            x = glue("Incompatible types for column `{name}`: {vec_ptype_full(x_i)} vs {vec_ptype_full(y_i)}.")
          )
        }
      )
    } else {
      if (!identical(vec_ptype(x_i), vec_ptype(y_i))) {
        msg <- c(
          msg,
          x = glue("Different types for column `{name}`: {vec_ptype_full(x_i)} vs {vec_ptype_full(y_i)}.")
        )
      }
    }
  }
  if (length(msg)) {
    return(msg)
  }

  TRUE
}

check_compatible <- function(x, y, ignore_col_order = TRUE, convert = TRUE, error_call = caller_env()) {
  compat <- is_compatible(x, y, ignore_col_order = ignore_col_order, convert = convert)
  if (isTRUE(compat)) {
    return()
  }

  abort(c("`x` and `y` are not compatible.", compat), call = error_call)
}
