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

group_indices_.data.frame <- function(.data, ..., .dots ){
  n <- nrow(.data)
  res <- integer(n)
  indices <- attr(data, "indices")
  ngroups <- length(indices)
  for(i in seq_len(ngroups)){
    res[ indices[[i]] + 1 ] <- i
  }
  res
}

