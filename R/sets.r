#' Set operations
#'
#' These functions override the set functions provided in base to make them
#' generic so that efficient versions for data frames and other tables can be
#' provided. The default methods call the base versions. Beware that
#' `intersect()`, `union()` and `setdiff()` remove duplicates.
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
#'
#' # Handling of duplicates:
#' a <- data.frame(column = c(1:10, 10))
#' b <- data.frame(column = c(1:5, 5))
#'
#' # intersection is 1 to 5, duplicates removed (5)
#' intersect(a, b)
#'
#' # union is 1 to 10, duplicates removed (5 and 10)
#' union(a, b)
#'
#' # set difference, duplicates removed (10)
#' setdiff(a, b)
#'
#' # union all does not remove duplicates
#' union_all(a, b)
NULL

#' @rdname setops
#' @importFrom generics intersect
#' @export
NULL

#' @rdname setops
#' @importFrom generics union
#' @export
NULL

#' @rdname setops
#' @export
union_all <- function(x, y, ...) UseMethod("union_all")

#' @importFrom generics setdiff
#' @rdname setops
#' @export
NULL

#' @importFrom generics setequal
#' @rdname setops
#' @export
NULL

#' @export
intersect.default <- function(x, y, ...) base::intersect(x, y, ...)
#' @export
union.default <- function(x, y, ...) base::union(x, y, ...)
#' @export
union_all.default <- function(x, y, ...) vec_c(x, y, ...)
#' @export
setdiff.default <- function(x, y, ...) base::setdiff(x, y, ...)
#' @export
setequal.default <- function(x, y, ...) base::setequal(x, y, ...)
