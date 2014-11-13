#' Group id.
#'
#' Generate a unique id for each group
#'
#' @seealso \code{\link{group_by}} 
#' @param .data a tbl
#' @param ... variables to group by. 
#' @export
#' @examples
#' group_indices(mtcars, cyl)
group_indices <- function(.data, ...) {
  group_indices_(.data, .dots = lazyeval::lazy_dots(...) )
}

#' @export
#' @rdname group_indices
group_indices_ <- function(.data, ..., .dots, add = FALSE) {
  UseMethod("group_indices_")
}

#' @export
group_indices_.data.frame <- function(.data, ..., .dots ){
  group_indices_(group_by(.data, ..., .dots))
}

#' @export
group_indices_.grouped_df <- function(.data, ..., .dots ){
  if( length(...) || length(.dots) ){
    warning( "group_indices_.grouped_df ignores extra arguments" )
  }
  grouped_indices_grouped_df_impl(.data)
}

