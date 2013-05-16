#' Group a data source by one or more variables.
#'
#' See \code{\link{ungroup}} for the opposite operation.
#'
#' @param x a data source
#' @param ... variables to group by. All data sources accept variable names,
#'   some will also accept functons of variables.
#' @export
group_by <- function(x, ...) {
  UseMethod("group_by")
}

#' Ungroup a grouped data source
#'
#' The inverse of \code{\link{group_by}}
#'
#' @param x a grouped data source
#' @export
ungroup <- function(x) {
  UseMethod("ungroup")
}

is.lazy <- function(x) {
  UseMethod("is.lazy")
}
