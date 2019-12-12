#' Join two tbls together
#'
#' These are generic functions that dispatch to individual tbl methods - see the
#' method documentation for details of individual data sources. `x` and
#' `y` should usually be from the same data source, but if `copy` is
#' `TRUE`, `y` will automatically be copied to the same source as `x`.
#'
#' @section Join types:
#'
#' Currently dplyr supports four types of mutating joins, two types of filtering joins, and
#' a nesting join.
#'
#' \strong{Mutating joins} combine variables from the two data.frames:
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
#'    \item{`full_join()`}{return all rows and all columns from both `x` and `y`.
#'    Where there are not matching values, returns `NA` for the one missing.}
#' }
#'
#'
#' \strong{Filtering joins} keep cases from the left-hand data.frame:
#'
#' \describe{
#'    \item{`semi_join()`}{return all rows from `x` where there are matching
#'    values in `y`, keeping just columns from `x`.
#'
#'    A semi join differs from an inner join because an inner join will return
#'    one row of `x` for each matching row  of `y`, where a semi
#'    join will never duplicate rows of `x`.}
#'
#'    \item{`anti_join()`}{return all rows from `x` where there are not
#'    matching values in `y`, keeping just columns from `x`.}
#' }
#'
#' \strong{Nesting joins} create a list column of data.frames:
#'
#' \describe{
#'    \item{`nest_join()`}{return all rows and all columns from `x`. Adds a
#'    list column of tibbles. Each tibble contains all the rows from `y`
#'    that match that row of `x`. When there is no match, the list column is
#'    a 0-row tibble with the same column names and types as `y`.
#'
#'    `nest_join()` is the most fundamental join since you can recreate the other joins from it.
#'    An `inner_join()` is a `nest_join()` plus an [tidyr::unnest()], and `left_join()` is a
#'    `nest_join()` plus an `unnest(.drop = FALSE)`.
#'    A `semi_join()` is a `nest_join()` plus a `filter()` where you check that every element of data has
#'    at least one row, and an `anti_join()` is a `nest_join()` plus a `filter()` where you check every element has zero rows.
#'    }
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
#'   `y`, these suffixes will be added to the output to disambiguate them.
#'   Should be a character vector of length 2.
#' @param name the name of the list column nesting joins create. If `NULL` the name of `y` is used.
#' @param keep If `TRUE` the by columns are kept in the nesting joins.
#' @param ... other parameters passed onto methods, for instance, `na_matches`
#'   to control how `NA` values are matched.  See \link{join.tbl_df} for more.
#' @name join
#' @examples
#' # "Mutating" joins combine variables from the LHS and RHS
#' band_members %>% inner_join(band_instruments)
#' band_members %>% left_join(band_instruments)
#' band_members %>% right_join(band_instruments)
#' band_members %>% full_join(band_instruments)
#'
#' # "Filtering" joins keep cases from the LHS
#' band_members %>% semi_join(band_instruments)
#' band_members %>% anti_join(band_instruments)
#'
#' # "Nesting" joins keep cases from the LHS and nests the RHS
#' band_members %>% nest_join(band_instruments)
#'
#' # To suppress the message, supply by
#' band_members %>% inner_join(band_instruments, by = "name")
#' # This is good practice in production code
#'
#' # Use a named `by` if the join variables have different names
#' band_members %>% full_join(band_instruments2, by = c("name" = "artist"))
#' # Note that only the key from the LHS is kept
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
nest_join <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  UseMethod("nest_join")
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
    bad_args("by", "can't contain join column {missing} which is missing from LHS",
      missing = fmt_obj(setdiff(by$x, x_vars))
    )
  }

  y_vars <- tbl_vars(y)
  if (!all(by$y %in% y_vars)) {
    bad_args("by", "can't contain join column {missing} which is missing from RHS",
      missing = fmt_obj(setdiff(by$y, y_vars))
    )
  }

  by
}

#' @export
common_by.NULL <- function(by, x, y) {
  by <- intersect(tbl_vars(x), tbl_vars(y))
  by <- by[!is.na(by)]
  if (length(by) == 0) {
    bad_args("by", "required, because the data sources have no common variables")
  }
  inform(auto_by_msg(by))

  list(
    x = by,
    y = by
  )
}

auto_by_msg <- function(by) {
  by_quoted <- encodeString(by, quote = '"')
  if (length(by_quoted) == 1L) {
    by_code <- by_quoted
  } else {
    by_code <- paste0("c(", paste(by_quoted, collapse = ", "), ")")
  }
  paste0("Joining, by = ", by_code)
}

#' @export
common_by.default <- function(by, x, y) {
  bad_args("by", "must be a (named) character vector, list, or NULL for ",
    "natural joins (not recommended in production code), not {friendly_type_of(by)}"
  )
}

check_suffix <- function(x) {
  if (!is.character(x) || length(x) != 2) {
    bad_args("suffix", "must be a character vector of length 2, ",
      "not {friendly_type_of(x)} of length {length(x)}"
    )
  }

  if (any(is.na(x))) {
    bad_args("suffix", "can't be NA")
  }

  if (all(x == "")) {
    bad_args("suffix", "can't be empty string for both `x` and `y` suffixes")
  }

  list(x = x[[1]], y = x[[2]])
}
