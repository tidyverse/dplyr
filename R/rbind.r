#' Efficiently bind multiple data frames by row and column.
#'
#' This is an efficient implementation of the common pattern of
#' \code{do.call(rbind, dfs)} or \code{do.call(cbind, dfs)} for binding many
#' data frames into one. \code{combine()} acts like \code{\link{c}()} or
#' \code{\link{unlist}()} but uses consistent dplyr coercion rules.
#'
#' @section Deprecated functions:
#' \code{rbind_list} and \code{rbind_all} have been deprecated. Instead use
#' \code{bind_rows}.
#'
#' @param x,... Data frames to combine.
#'
#'   You can either supply one data frame per argument, or a list of
#'   data frames in the first argument.
#'
#'   When column-binding, rows are matched by position, not value so all data
#'   frames must have the same number of rows. To match by value, not
#'   position, see \code{left_join} etc. When row-binding, columns are
#'   matched by name, and any values that don't match will be filled with NA.
#' @return \code{bind_rows} and \code{bind_cols} always return a \code{tbl_df}
#' @aliases rbind_all rbind_list
#' @examples
#' one <- mtcars[1:4, ]
#' two <- mtcars[11:14, ]
#'
#' # You can either supply data frames as arguments
#' bind_rows(one, two)
#' # Or a single argument containing a list of data frames
#' bind_rows(list(one, two))
#' bind_rows(split(mtcars, mtcars$cyl))
#'
#' # Columns don't need to match when row-binding
#' bind_rows(data.frame(x = 1:3), data.frame(y = 1:4))
#' \dontrun{
#' # Rows do need to match when column-binding
#' bind_cols(data.frame(x = 1), data.frame(y = 1:2))
#' }
#'
#' bind_cols(one, two)
#' bind_cols(list(one, two))
#'
#' # combine applies the same coercion rules
#' f1 <- factor("a")
#' f2 <- factor("b")
#' c(f1, f2)
#' unlist(list(f1, f2))
#'
#' combine(f1, f2)
#' combine(list(f1, f2))
#' @name bind
NULL


#' @export
#' @rdname bind
bind_rows <- function(x, ...) {
  if (is.list(x) && !is.data.frame(x) && !length(list(...)) ) {
    rbind_all(x)
  } else {
    rbind_all(list(x, ...))
  }
}

#' @export
#' @rdname bind
bind_cols <- function(x, ...) {
  if (is.list(x) && !is.data.frame(x)) {
    cbind_all(x)
  } else {
    cbind_all(list(x, ...))
  }
}

#' @export
#' @rdname bind
combine <- function(x, ...) {
  if (is.list(x) && !is.data.frame(x)) {
    combine_all(x)
  } else {
    combine_all(list(x, ...))
  }
}


#' @export
rbind_list <- function(...){
  rbind_list__impl(environment())
}
