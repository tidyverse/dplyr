#' Set operations
#'
#' These functions override the set functions provided in base to make them
#' generic so that efficient versions for data frames and other tables can be
#' provided. The default methods call the base versions. Beware that
#' `intersect()`, `union()` and `setdiff()` remove duplicates.
#'
#' @param x,y objects to perform set function on (ignoring order)
#' @param ... other arguments passed on to methods
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
#' # Handling of duplicates:
#' a <- data.frame(column = c(1:10, 10))
#' b <- data.frame(column = c(1:5, 5))
#'
#' # intersection is 1 to 5, duplicates removed (5)
#' intersect(a, b)
#'
#' # union is 1 to 10, duplicates removed (5 and 10)
#' union(a, b)
#'
#' # set difference, duplicates removed (10)
#' setdiff(a, b)
#'
#' # union all does not remove duplicates
#' union_all(a, b)
NULL

#' @rdname setops
#' @export
intersect <- function(x, y, ...) UseMethod("intersect")
#' @rdname setops
#' @export
union <- function(x, y, ...) UseMethod("union")
#' @rdname setops
#' @export
union_all <- function(x, y, ...) UseMethod("union_all")
#' @rdname setops
#' @export
setdiff <- function(x, y, ...) UseMethod("setdiff")
#' @rdname setops
#' @export
setequal <- function(x, y, ...) UseMethod("setequal")

#' @export
intersect.default <- function(x, y, ...) base::intersect(x, y, ...)
#' @export
union.default <- function(x, y, ...) base::union(x, y, ...)
#' @export
union_all.default <- function(x, y, ...) vec_c(x, y, ...)
#' @export
setdiff.default <- function(x, y, ...) base::setdiff(x, y, ...)
#' @export
setequal.default <- function(x, y, ...) base::setequal(x, y, ...)


#' @export
intersect.data.frame <- function(x, y, ...) {
  check_compatible(x, y)
  original_x <- x
  c(x, y) %<-% vec_cast_common(x, y)
  out <- vec_unique(vec_slice(x, vec_in(x, y)))
  reconstruct_set(out, original_x)
}

#' @export
union.data.frame <- function(x, y, ...) {
  check_compatible(x, y)
  out <- vec_unique(vec_rbind(!!!vec_cast_common(x, y)))
  reconstruct_set(out, x)
}

#' @export
union_all.data.frame <- function(x, y, ...) {
  out <- bind_rows(x, y)
  reconstruct_set(out, x)
}

#' @export
setdiff.data.frame <- function(x, y, ...) {
  check_compatible(x, y)
  original_x <- x
  c(x, y) %<-% vec_cast_common(x, y)
  out <- vec_unique(vec_slice(x, !vec_in(x, y)))
  reconstruct_set(out, original_x)
}

#' @export
setequal.data.frame <- function(x, y, ...) {
  isTRUE(equal_data_frame(x, y))
}

reconstruct_set <- function(out, x) {
  if (is_grouped_df(x)) {
    out <- grouped_df(out, group_vars(x), group_by_drop_default(x))
  }

  out
}


# Helpers -----------------------------------------------------------------



is_compatible_data_frame <- function(x, y, ignore_col_order = TRUE, convert = TRUE) {
  nc <- ncol(x)
  if (nc != ncol(y)) {
    return(glue("- different number of columns : {nc} vs {ncol(y)}"))
  }

  names_x <- names(x)
  names_y <- names(y)

  names_y_not_in_x <- setdiff(names_y, names_x)
  names_x_not_in_y <- setdiff(names_x, names_y)

  if (length(names_y_not_in_x) == 0L && length(names_x_not_in_y) == 0L) {
    # check if same order
    if (!isTRUE(ignore_col_order)) {
      if (!identical(names_x, names_y)) {
        return("- Same column names, but different order")
      }
    }
  } else {
    # names are not the same, explain why

    msg <- "not compatible: \n"
    if (length(names_y_not_in_x)) {
      msg <- paste0(msg, "- Cols in y but not x: ", glue_collapse(glue('`{names_y_not_in_x}`'), sep = ", "), ".\n")
    }
    if (length(names_x_not_in_y)) {
      msg <- paste0(msg, "- Cols in x but not y: ", glue_collapse(glue('`{names_x_not_in_y}`'), sep = ", "), ".\n")
    }
    return(msg)
  }

  msg <- ""
  for (name in names_x) {
    x_i <- x[[name]]
    y_i <- y[[name]]

    if (convert) {
      tryCatch(
        vec_ptype2(x_i, y_i),
        error = function(e) {
          msg <<- paste0(msg,
            glue("- Incompatible types for column `{name}`: {vec_ptype_full(x_i)} vs {vec_ptype_full(y_i)}"),
            "\n"
          )
        }
      )
    } else {
      if (!identical(vec_ptype(x_i), vec_ptype(y_i))) {
        msg <- paste0(msg,
          glue("- Different types for column `{name}`: {vec_ptype_full(x_i)} vs {vec_ptype_full(y_i)}"),
          "\n"
        )
      }
    }
  }
  if (msg != "") {
    return(msg)
  }

  TRUE
}

check_compatible <- function(x, y, ignore_col_order = TRUE, convert = TRUE) {
  compat <- is_compatible_data_frame(x, y, ignore_col_order = ignore_col_order, convert = convert)
  if (is.character(compat)) {
    abort(paste0("not compatible: \n", glue_collapse(compat, sep = "\n")))
  }
}
