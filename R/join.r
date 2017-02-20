#' Join two tbls together.
#'
#' These are generic functions that dispatch to individual tbl methods - see the
#' method documentation for details of individual data sources. `x` and
#' `y` should usually be from the same data source, but if `copy` is
#' `TRUE`, `y` will automatically be copied to the same source as
#' `x` - this may be an expensive operation.
#'
#' @section Join types:
#'
#' Currently dplyr supports four join types:
#'
#' \describe{
#'    \item{`inner_join()`}{return all rows from `x` where there are matching
#'    values in `y`, and all columns from `x` and `y`. If there are multiple matches
#'    between `x` and `y`, all combination of the matches are returned.}
#'
#'    \item{`left_join()`}{return all rows from `x`, and all columns from `x`
#'    and `y`. Rows in `x` with no match in `y` will have `NA` values in the new
#'    columns. If there are multiple matches between `x` and `y`, all combinations
#'    of the matches are returned.}
#'
#'   \item{`right_join()`}{return all rows from `y`, and all columns from `x`
#'    and y. Rows in `y` with no match in `x` will have `NA` values in the new
#'    columns. If there are multiple matches between `x` and `y`, all combinations
#'    of the matches are returned.}
#'
#'    \item{`semi_join()`}{return all rows from `x` where there are matching
#'    values in `y`, keeping just columns from `x`.
#'
#'    A semi join differs from an inner join because an inner join will return
#'    one row of `x` for each matching row  of `y`, where a semi
#'    join will never duplicate rows of `x`.}
#'
#'    \item{`anti_join()`}{return all rows from `x` where there are not
#'    matching values in `y`, keeping just columns from `x`.}
#'
#'    \item{`full_join()`}{return all rows and all columns from both `x` and `y`.
#'    Where there are not matching values, returns `NA` for the one missing.}
#' }
#'
#' @section Grouping:
#'
#' Groups are ignored for the purpose of joining, but the result preserves
#' the grouping of `x`.
#'
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If `NULL`, the
#'   default, `*_join()` will do a natural join, using all variables with
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right (to suppress the message, simply
#'   explicitly list the variables that you want to join).
#'
#'   To join by different variables on x and y use a named vector.
#'   For example, `by = c("a" = "b")` will match `x.a` to
#'   `y.b`.
#' @param copy If `x` and `y` are not from the same data source,
#'   and `copy` is `TRUE`, then `y` will be copied into the
#'   same src as `x`.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param suffix If there are non-joined duplicate variables in `x` and
#'   `y`, these suffixes will be added to the output to diambiguate them.
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
common_by <- function(by = NULL, x, y) UseMethod("common_by", by)

#' @export
common_by.character <- function(by, x, y) {
  by <- common_by_from_vector(by)
  common_by.list(by, x, y)
}

common_by_from_vector <- function(by) {
  by <- by[!duplicated(by)]
  by_x <- names(by) %||% by
  by_y <- unname(by)

  # If x partially named, assume unnamed are the same in both tables
  by_x[by_x == ""] <- by_y[by_x == ""]

  list(x = by_x, y = by_y)
}

#' @export
common_by.list <- function(by, x, y) {
  x_vars <- tbl_vars(x)
  if (!all(by$x %in% x_vars)) {
    stop(
      "Join column not found in lhs: ",
      paste(setdiff(by$x, x_vars), collapse = ", "),
      call. = FALSE
    )
  }

  y_vars <- tbl_vars(y)
  if (!all(by$y %in% y_vars)) {
    stop(
      "Join column not found in rhs: ",
      paste(setdiff(by$y, y_vars), collapse = ", "),
      call. = FALSE
    )
  }

  by
}

#' @export
common_by.NULL <- function(by, x, y) {
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

#' @export
common_by.default <- function(by, x, y) {
  stop(
    "`by` must be a (named) character vector, a list, or NULL for ",
    "natural joins (not recommended in production code)",
    call. = FALSE
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
