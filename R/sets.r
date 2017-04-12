#' Set operations
#'
#' These functions override the set functions provided in base to make them
#' generic so that efficient versions for data frames and other tables can be
#' provided. The default methods call the base versions.
#'
#' @param x,y objects to perform set function on (ignoring order)
#' @param ... other arguments passed on to methods
#' @name setops
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
#' union_all(first, second)
#' setequal(mtcars, mtcars[32:1, ])
NULL

#' @rdname setops
#' @export
intersect <- function(x, y, ...) UseMethod("intersect")
#' @rdname setops
#' @export
union <- function(x, y, ...) UseMethod("union")
#' @rdname setops
#' @export
union_all <- function(x, y, ...) UseMethod("union_all")
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
union_all.default <- function(x, y, ...) combine(x, y, ...)
#' @export
setdiff.default <-   function(x, y, ...) base::setdiff(x, y, ...)
#' @export
setequal.default <-  function(x, y, ...) base::setequal(x, y, ...)
