#' Flexible equality comparison for data frames.
#'
#' When comparing two \code{tbl_df} using \code{\link{all.equal}}, column and
#' row order is ignored by default, and types are not coerced.  The \code{dplyr}
#' package provides a much more efficient implementation for this funcitonality.
#'
#' @param target,current Two data frames to compare.
#' @param ignore_col_order Should order of columns be ignored?
#' @param ignore_row_order Should order of rows be ignored?
#' @param convert Should similar classes be converted? Currently this will
#'   convert factor to character and integer to double.
#' @param ... Ignored. Needed for compatibility with \code{all.equal}.
#' @return \code{TRUE} if equal, otherwise a character vector describing
#'   the reasons why they're not equal. Use \code{\link{isTRUE}} if using the
#'   result in an \code{if} expression.
#' @examples
#' scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
#' mtcars_df <- as_data_frame(mtcars)
#'
#' # By default, ordering of rows and columns ignored
#' all.equal(mtcars_df, scramble(mtcars_df))
#'
#' # But those can be overriden if desired
#' all.equal(mtcars_df, scramble(mtcars_df), ignore_col_order = FALSE)
#' all.equal(mtcars_df, scramble(mtcars_df), ignore_row_order = FALSE)
#'
#' # By default all.equal is sensitive to variable differences
#' df1 <- data_frame(x = "a")
#' df2 <- data_frame(x = factor("a"))
#' all.equal(df1, df2)
#' # But you can request dplyr convert similar types
#' all.equal(df1, df2, convert = TRUE)
all_equal <- function(target, current, ignore_col_order = TRUE,
                      ignore_row_order = TRUE, convert = FALSE, ...) {

  if (!identical(class(target), class(current))) {
    return(paste0("Different types: x ", paste(class(target), collapse = ", "),
                  ", y ", paste(class(current), collapse = ", ")))
  }
  if (nrow(target) != nrow(current)) {
    return("Different number of rows")
  }
  extra_x <- setdiff(names(target), names(current))
  if (length(extra_x) > 0L) {
    return(paste0("Cols in x but not y: ", paste(extra_x, collapse = ", ")))
  }
  extra_y <- setdiff(names(current), names(target))
  if (length(extra_y) > 0L) {
    return(paste0("Cols in y but not x: ", paste(extra_y, collapse = ", ")))
  }
  if (!ignore_col_order && names(target) != names(current)) {
    return("Column names same but in different order")
  }

  current <- `rownames<-`(current[names(target)], rownames(current))

  types <- unlist(mapply(
    function(x, y) {
      if (!identical(class(x), class(y))) {
        paste0("x ", class(x), ", y ", class(y))
      }
    },
    target, current
  ))

  if (length(types) > 0L) {
    types <- paste0("Incompatible type for column ", names(types), ": ", types)
    if (convert) {
      lapply(types, warning, call. = FALSE)
    } else {
      return(types)
    }
  }

  factor_levels <- unlist(mapply(
    function(x, y) {
      if (!identical(levels(x), levels(y))) {
        TRUE
      }
    },
    target, current
  ))

  if (length(factor_levels) > 0L) {
    return(paste0("Factor levels not equal for column ", names(factor_levels)))
  }

  if (ignore_row_order) {
    target <- target[do.call(order, target), ]
    current <- current[do.call(order, current), ]
  }

  all.equal(as.data.frame(target), as.data.frame(current), ...)
}

#' @export
#' @rdname all_equal
#' @method all.equal tbl_df
all.equal.tbl_df <- all_equal
