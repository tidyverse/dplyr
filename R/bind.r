#' Efficiently bind multiple data frames by row and column.
#'
#' This is an efficient implementation of the common pattern of
#' \code{do.call(rbind, dfs)} or \code{do.call(cbind, dfs)} for binding many
#' data frames into one. \code{combine()} acts like \code{\link{c}()} or
#' \code{\link{unlist}()} but uses consistent dplyr coercion rules.
#'
#' @section Deprecated functions:
#' \code{rbind_list()} and \code{rbind_all()} have been deprecated. Instead use
#' \code{bind_rows()}.
#'
#' @param ... Data frames to combine.
#'
#'   Each argument can either be a data frame, a list that could be a data
#'   frame, or a list of data frames.
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
#' @return \code{bind_rows} and \code{bind_cols} return the same type as
#'   the first input, either a data frame, \code{tbl_df}, or \code{grouped_df}.
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

#' @export
#' @rdname bind
bind_rows <- function(..., .id = NULL) {
  x <- list_or_dots(...)

  if (!is.null(.id)) {
    if (!(is.character(.id) && length(.id) == 1)) {
      stop(".id is not a string", call. = FALSE)
    }
    names(x) <- names(x) %||% seq_along(x)
  }

  bind_rows_(x, .id)
}


#' @export
#' @rdname bind
bind_cols <- function(...) {
  x <- list_or_dots(...)
  cbind_all(x)
}

#' @export
#' @rdname bind
combine <- function(...) {
  x <- list_or_dots(...)
  combine_all(x)
}

list_or_dots <- function(...) {
  dots <- list(...)

  # Need to ensure that each component is a data list:
  data_lists <- vapply(dots, is_data_list, logical(1))
  dots[data_lists] <- lapply(dots[data_lists], list)

  unlist(dots, recursive = FALSE)
}

# Is this object a
is_data_list <- function(x) {
  # data frames are trivially data list, and so are nulls
  if (is.data.frame(x) || is.null(x))
    return(TRUE)

  # Must be a list
  if (!is.list(x))
    return(FALSE)

  # 0 length named list (#1515)
  if( !is.null(names(x)) && length(x) == 0)
    return(TRUE)

  # With names
  if (any(!has_names(x)))
    return(FALSE)

  # Where each element is an 1d vector or list
  is_1d <- vapply(x, is_1d, logical(1))
  if (any(!is_1d))
    return(FALSE)

  # All of which have the same length
  n <- vapply(x, length, integer(1))
  if (any(n != n[1]))
    return(FALSE)

  TRUE
}


# Deprecated functions ----------------------------------------------------

#' @export
#' @rdname bind
#' @usage NULL
rbind_list <- function(...){
  warning("`rbind_list()` is deprecated. Please use `bind_rows()` instead.",
    call. = FALSE)
  rbind_list__impl(environment())
}

#' @export
#' @rdname bind
#' @usage NULL
rbind_all <- function(x, id = NULL) {
  warning("`rbind_all()` is deprecated. Please use `bind_rows()` instead.",
    call. = FALSE)
  bind_rows_(x, id = id)
}
