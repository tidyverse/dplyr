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
#' @param .id Data frames identifier.
#'
#'   When \code{.id} is supplied, a new column of identifiers is
#'   created to link each row to its original data frame. The labels
#'   are taken from the named arguments to \code{bind_rows()}. When a
#'   list of data frames is supplied, the labels are taken from the
#'   names of the list. If no names are found a numeric sequence is
#'   used instead.
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
#' # When you supply a column name with the `.id` argument, a new
#' # column is created to link each row to its original data frame
#' bind_rows(list(one, two), .id = "id")
#' bind_rows(list(a = one, b = two), .id = "id")
#' bind_rows("group 1" = one, "group 2" = two, .id = "groups")
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

is_dataframe_able <- function(x){
  inherits( x, "data.frame") ||
    is.null(x) ||
    ( is.list(x) && !is.null(names(x)) && do.call( all.equal, unname(lapply(x, length)) ) )
}

#' @export
#' @rdname bind
bind_rows <- function(..., .id = NULL) {
  dots <- list(...)
  if (is.list(dots[[1]]) && !is_dataframe_able(dots[[1]]) && !length(dots[-1])) {
    x <- dots[[1]]
  }
  else {
    x <- dots
  }

  if (!is.null(.id)) {
    if (!(is.character(.id) && length(.id) == 1)) {
      stop(".id is not a string", call. = FALSE)
    }
    names(x) <- names(x) %||% seq_along(x)
  }

  rbind_all(x, .id)
}

#' @export
#' @rdname bind
bind_cols <- function(x, ...) {
  if (is.list(x) && !is.data.frame(x) && !length(list(...)) ) {
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
