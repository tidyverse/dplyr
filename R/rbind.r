#' Efficiently bind multiple data frames by row and column.
#'
#' This is an efficient implementation of the common pattern of
#' \code{do.call(rbind, dfs)} or \code{do.call{cbind, dfs}} for binding many
#' data frames into one. \code{combine()} acts like \code{\link{c}()} or
#' \code{\link{unlist}()} but uses consistent dplyr coercion rules.
#'
#' @section Deprecated functions:
#' \code{rbind_list} and \code{rbind_all} have been deprecated. Instead use
#' \code{bind_rows}.
#'
#' @param ...,dots Data frames or lists of data frames to combine.
#'
#'   You can supply data frames and lists of data frames indistinctly.
#'
#'   When column-binding, rows are matched by position, not value so all data
#'   frames must have the same number of rows. To match by value, not
#'   position, see \code{left_join} etc. When row-binding, columns are
#'   matched by name, and any values that don't match will be filled with NA.
#' @return \code{bind_rows} and \code{bind_cols} always return a \code{tbl_df}
#' @examples
#' one <- mtcars[1:4, ]
#' two <- mtcars[11:14, ]
#'
#' # You can supply data frames as arguments
#' bind_rows(one, two)
#' # Or lists of data frames
#' bind_rows(list(one, two))
#' bind_rows(list(one, two), list(two, one, one))
#' bind_rows(split(mtcars, mtcars$cyl))
#' # Or any combination thereof
#' bind_rows(mtcars, list(one, two), two)
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
bind_rows <- function(...) {
  rbind_all(enlist(...))
}

#' @export
#' @rdname bind
bind_cols <- function(...) {
  cbind_all(enlist(...))
}

#' @export
#' @rdname bind
combine <- function(...) {
  combine_all(enlist(...))
}


#' @export
#' @rdname bind
rbind_list <- function(...){
  rbind_list__impl(environment())
}
