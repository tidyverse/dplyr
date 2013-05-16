#' Group a data source by one or more variables.
#'
#' Most data operations are useful done on groups defined by variables in the
#' the dataset. The \code{group_by} function takes an existing data source
#' and converts it into a grouped data source where operations are performed
#' "by group".
#'
#' @section Data sources
#'
#' \code{group_by} is an S3 generic with methods for the three built-in
#' data sources. See the help for the corresponding classes and their manip
#' methods for more details:
#'
#' \itemize{
#'   \item data.frame: \link{grouped_df}, \link{manip_grouped_df}
#'   \item data.table: \link{grouped_dt}, \link{manip_grouped_dt}
#'   \item SQLite: \link{grouped_sqlite}, \link{manip_grouped_sqlite}
#' }
#'
#' @seealso \code{\link{ungroup}} for the inverse operation.
#' @param x a data source
#' @param ... variables to group by. All data sources accept variable names,
#'   some will also accept functons of variables.
#' @export
#' @examples
#' by_cyl <- group_by(mtcars, cyl)
#' summarise(by_cyl, mean(disp), mean(hp))
#' filter(by_cyl, disp == max(disp))
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
