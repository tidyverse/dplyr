#' Data manipulation functions.
#'
#' @name manip
NULL

#' @rdname manip
#' @export
filter <- function(.data, ...) UseMethod("filter")

#' @rdname manip
#' @export
summarise <- function(.data, ...) UseMethod("summarise")

#' @rdname manip
#' @export
mutate <- function(.data, ...) UseMethod("mutate")

#' @rdname manip
#' @export
arrange <- function(.data, ...) UseMethod("arrange")

#' @rdname manip
#' @export
select <- function(.data, ...) UseMethod("select")

#' @rdname manip
#' @export
group <- function(.data, ...) UseMethod("group")
