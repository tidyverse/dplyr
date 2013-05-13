#' Data manipulation functions.
#'
#' @name manip
NULL

#' @rdname manip
#' @export
filter <- function(`_data`, ...) UseMethod("filter")

#' @rdname manip
#' @export
summarise <- function(`_data`, ...) UseMethod("summarise")

#' @rdname manip
#' @export
mutate <- function(`_data`, ...) UseMethod("mutate")

#' @rdname manip
#' @export
arrange <- function(`_data`, ...) UseMethod("arrange")

#' @rdname manip
#' @export
select <- function(`_data`, ...) UseMethod("select")

#' @rdname manip
#' @export
limit <- function(`_data`, n) UseMethod("limit")


