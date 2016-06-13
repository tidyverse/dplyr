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
#'    \item{\code{inner_join}}{return all rows from \code{x} where there are matching
#'    values in \code{y}, and all columns from \code{x} and \code{y}. If there are multiple matches
#'    between \code{x} and \code{y}, all combination of the matches are returned.}
#'
#'    \item{\code{left_join}}{return all rows from \code{x}, and all columns from \code{x}
#'    and \code{y}. Rows in \code{x} with no match in \code{y} will have \code{NA} values in the new
#'    columns. If there are multiple matches between \code{x} and \code{y}, all combinations
#'    of the matches are returned.}
#'
#'   \item{\code{right_join}}{return all rows from \code{y}, and all columns from \code{x}
#'    and y. Rows in \code{y} with no match in \code{x} will have \code{NA} values in the new
#'    columns. If there are multiple matches between \code{x} and \code{y}, all combinations
#'    of the matches are returned.}
#'
#'    \item{\code{semi_join}}{return all rows from \code{x} where there are matching
#'    values in \code{y}, keeping just columns from \code{x}.
#'
#'    A semi join differs from an inner join because an inner join will return
#'    one row of \code{x} for each matching row  of \code{y}, where a semi
#'    join will never duplicate rows of \code{x}.}
#'
#'    \item{\code{anti_join}}{return all rows from \code{x} where there are not
#'    matching values in \code{y}, keeping just columns from \code{x}.}
#'
#'    \item{\code{full_join}}{return all rows and all columns from both \code{x} and \code{y}.
#'    Where there are not matching values, returns \code{NA} for the one missing.}
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
#'   that you can check they're right (to suppress the message, simply
#'   explicitly list the variables that you want to join).
#'
#'   To join by different variables on x and y use a named vector.
#'   For example, \code{by = c("a" = "b")} will match \code{x.a} to
#'   \code{y.b}.
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
#'   same src as \code{x}.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param suffix If there are non-joined duplicate variables in \code{x} and
#'   \code{y}, these suffixes will be added to the output to diambiguate them.
#' @param ... other parameters passed onto methods
#' @name join
NULL

#' @rdname join
#' @export
inner_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("inner_join")
}

#' @rdname join
#' @export
left_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("left_join")
}

#' @rdname join
#' @export
right_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("right_join")
}

#' @rdname join
#' @export
full_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("full_join")
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

#' Extract out common by variables
#'
#' @export
#' @keywords internal
common_by <- function(by = NULL, x, y) {
  if (is.list(by)) return(by)

  if (!is.null(by)) {
    by <- by[!duplicated(by)]
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
  message("Joining, by = ", utils::capture.output(dput(by)))

  list(
    x = by,
    y = by
  )
}

# Returns NULL if variables don't need to be renamed
unique_names <- function(x_names, y_names, by, suffix = c(".x", ".y")) {

  common <- setdiff(intersect(x_names, y_names), by$x[by$x == by$y])
  if (length(common) == 0) return(NULL)

  suffix <- check_suffix(suffix)

  x_match <- match(common, x_names)
  x_new <- x_names
  x_new[x_match] <- paste0(x_names[x_match], suffix$x)

  y_match <- match(common, y_names)
  y_new <- y_names
  y_new[y_match] <- paste0(y_names[y_match], suffix$y)

  list(x = setNames(x_new, x_names), y = setNames(y_new, y_names))
}

check_suffix <- function(x) {
  if (!is.character(x) || length(x) != 2) {
    stop("`suffix` must be a character vector of length 2.", call. = FALSE)
  }

  list(x = x[1], y = x[2])
}
