#' Join two tbls together.
#'
#' These are generic functions that dispatch to individual tbl methods - see the
#' method documentation for details of individual data sources. \code{x} and
#' \code{y} should usually be from the same data source, but if \code{copy} is
#' \code{TRUE}, \code{y} will automatically be copied to the same source as
#' \code{x} - this may be an expensive operation.
#'
#' @section Join types:
#'
#' Currently dplyr supports four join types:
#'
#' \describe{
#'    \item{\code{inner_join}}{return all rows from x where there are matching
#'    values in y, and all columns from x and y. If there are multiple matches
#'    between x and y, all combination of the matches are returned.}
#'
#'    \item{\code{left_join}}{return all rows from x, and all columns from x
#'    and y. Rows in x with no match in y will have NA values in the new
#'    columns. If there are multiple matches between x and y, all combinations
#'    of the matches are returned.}
#'
#'    \item{\code{semi_join}}{return all rows from x where there are matching
#'    values in y, keeping just columns from x.
#'
#'    A semi join differs from an inner join because an inner join will return
#'    one row of \code{x} for each matching row  of \code{y}, where a semi
#'    join will never duplicate rows of \code{x}.}
#'
#'    \item{\code{anti_join}}{return all rows from x where there are not
#'    matching values in y, keeping just columns from x}
#' }
#'
#' @section Grouping:
#'
#' Groups are ignored for the purpose of joining, but the result preserves
#' the grouping of \code{x}.
#'
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If \code{NULL}, the
#'   default, \code{join} will do a natural join, using all variables with
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right.
#'
#'   To join by different variables on x and y use a named vector.
#'   For example, \code{by = c("a" = "b")} will match \code{x.a} to
#'   \code{y.b}.
#'
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
#'   same src as \code{x}.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param ... other parameters passed onto methods
#' @name join
NULL

#' @rdname join
#' @export
inner_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("inner_join")
}

#' @rdname join
#' @export
left_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("left_join")
}

#' @rdname join
#' @export
right_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("right_join")
}

#' @rdname join
#' @export
outer_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("outer_join")
}

#' @rdname join
#' @export
semi_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("semi_join")
}

#' @rdname join
#' @export
anti_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("anti_join")
}

common_by <- function(by = NULL, x, y) {
  if (!is.null(by)) {
    x <- names(by) %||% by
    y <- unname(by)

    # If x partially named, assume unnamed are the same in both tables
    x[x == ""] <- y[x == ""]

    return(list(x = x, y = y))
  }

  by <- intersect(tbl_vars(x), tbl_vars(y))
  if (length(by) == 0) {
    stop("No common variables. Please specify `by` param.", call. = FALSE)
  }
  message("Joining by: ", capture.output(dput(by)))

  list(
    x = by,
    y = by
  )
}

unique_names <- function(x_names, y_names, by, x_suffix = ".x", y_suffix = ".y") {
  common <- setdiff(intersect(x_names, y_names), by)
  if (length(common) == 0) return(NULL)

  x_match <- match(common, x_names)
  x_new <- x_names
  x_new[x_match] <- paste0(x_names[x_match], x_suffix)

  y_match <- match(common, y_names)
  y_new <- y_names
  y_new[y_match] <- paste0(y_names[y_match], y_suffix)

  list(x = setNames(x_new, x_names), y = setNames(y_new, y_names))
}
