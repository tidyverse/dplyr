#' Set operations.
#'
#' These functions override the set functions provided in base to make them
#' generic so that efficient versions for data frames and other tables can be
#' provided. The default methods call the base versions.
#'
#' @param x,y objects to compare (ignoring order)
#' @param ... other arguments passed on to methods
#' @name setops
NULL

#' @rdname setops
#' @export
intersect <- function(x, y, ...) UseMethod("intersect")
#' @rdname setops
#' @export
union <- function(x, y, ...) UseMethod("union")
#' @rdname setops
#' @export
setdiff <- function(x, y, ...) UseMethod("setdiff")
#' @rdname setops
#' @export
setequal <- function(x, y, ...) UseMethod("setequal")

#' @export
intersect.default <- function(x, y, ...) base::intersect(x, y, ...)
#' @export
union.default <-     function(x, y, ...) base::union(x, y, ...)
#' @export
setdiff.default <-   function(x, y, ...) base::setdiff(x, y, ...)
#' @export
setequal.default <-  function(x, y, ...) base::setequal(x, y, ...)

#' Set operations for data frames.
#'
#' These set operations are implemented with an efficeint C++ backend.
#'
#' @param x,y Two data frames to compare, igoring order of row and columns
#' @param ... Needed for compatibility with generic. Otherwise ignored.
#'
#' @name setops-data.frame
#' @examples
#' mtcars$model <- rownames(mtcars)
#' first <- mtcars[1:20, ]
#' second <- mtcars[10:32, ]
#'
#' intersect(first, second)
#' union(first, second)
#' setdiff(first, second)
#' setdiff(second, first)
#'
#' setequal(mtcars, mtcars[32:1, ])
NULL

#' @rdname setops-data.frame
#' @export
intersect.data.frame <- function(x, y, ...) intersect_data_frame(x, y)
#' @rdname setops-data.frame
#' @export
union.data.frame <-     function(x, y, ...) union_data_frame(x, y)
#' @rdname setops-data.frame
#' @export
setdiff.data.frame <-   function(x, y, ...) setdiff_data_frame(x, y)
#' @rdname setops-data.frame
#' @export
setequal.data.frame <-  function(x, y, ...) equal_data_frame(x, y)
