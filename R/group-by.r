#' Group a data source by one or more variables.
#'
#' @param x a data source
#' @param ... variables to group by. All data sources accept variable names,
#'   some will also accept functons of variables.
#' @export
group_by <- function(x, ...) {
  UseMethod("group_by")
}

ungroup <- function(x) {
  UseMethod("ungroup")
}

is.lazy <- function(x) {
  UseMethod("is.lazy")
}
