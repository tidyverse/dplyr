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
anti_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("anti_join")
}

#' @rdname join
#' @export
nest_join <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  UseMethod("nest_join")
}


#' Join data frame tbls
#'
#' See [join] for a description of the general purpose of the
#' functions.
#'
#' @inheritParams inner_join
#' @param ... included for compatibility with the generic; otherwise ignored.
#' @param na_matches
#'   Use `"never"` to always treat two `NA` or `NaN` values as
#'   different, like joins for database sources, similarly to
#'   `merge(incomparables = FALSE)`.
#'   The default, `"na"`, always treats two `NA` or `NaN` values as equal, like [merge()].
#'   Users and package authors can change the default behavior by calling
#'   `pkgconfig::set_config("dplyr::na_matches" = "never")`.
#' @examples
#' if (require("Lahman")) {
#' batting_df <- as_tibble(Batting)
#' person_df <- as_tibble(Master)
#'
#' uperson_df <- as_tibble(Master[!duplicated(Master$playerID), ])
#'
#' # Inner join: match batting and person data
#' inner_join(batting_df, person_df)
#' inner_join(batting_df, uperson_df)
#'
#' # Left join: match, but preserve batting data
#' left_join(batting_df, uperson_df)
#'
#' # Anti join: find batters without person data
#' anti_join(batting_df, person_df)
#' # or people who didn't bat
#' anti_join(person_df, batting_df)
#' }
#' @name join.tbl_df
NULL

check_na_matches <- function(na_matches) {
  na_matches <- match.arg(na_matches, choices = c("na", "never"))
  accept_na_match <- (na_matches == "na")
  accept_na_match
}

#' @export
#' @rdname join.tbl_df
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {

  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by, suffix = suffix)
  y <- auto_copy(x, y, copy = copy)
  na_matches <- check_na_matches(na_matches)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  rows <- join_rows(x_key, y_key, type = "inner")

  x_out <- set_names(x[vars$x$out], names(vars$x$out))
  y_out <- set_names(y[vars$y$out], names(vars$y$out))

  out <- vec_slice(x_out, rows$x)
  out[names(x_key)] <- vec_cast(out[names(x_key)], vec_ptype2(x_key, y_key))
  out[names(y_out)] <- vec_slice(y_out, rows$y)
  out
}

#' @export
#' @rdname join.tbl_df
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by, suffix = suffix)
  y <- auto_copy(x, y, copy = copy)
  na_matches <- check_na_matches(na_matches)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  rows <- join_rows(x_key, y_key, type = "left")

  x_out <- set_names(x[vars$x$out], names(vars$x$out))
  y_out <- set_names(y[vars$y$out], names(vars$y$out))

  out <- vec_slice(x_out, rows$x)
  out[names(x_key)] <- vec_cast(out[names(x_key)], vec_ptype2(x_key, y_key))
  out[names(y_out)] <- vec_slice(y_out, rows$y)
  out
}

#' @export
#' @rdname join.tbl_df
right_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by, suffix = suffix)
  y <- auto_copy(x, y, copy = copy)
  na_matches <- check_na_matches(na_matches)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  rows <- join_rows(x_key, y_key, type = "right")

  x_out <- set_names(x[vars$x$out], names(vars$x$out))
  y_out <- set_names(y[vars$y$out], names(vars$y$out))

  out <- vec_slice(x_out, c(rows$x, rep_along(rows$y_extra, NA_integer_)))
  out[names(x_key)] <- vec_rbind(
    vec_slice(x_key, rows$x),
    vec_slice(y_key, rows$y_extra)
  )
  out[names(y_out)] <- vec_slice(y_out, c(rows$y, rows$y_extra))
  out
}

#' @export
#' @rdname join.tbl_df
full_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by, suffix = suffix)
  y <- auto_copy(x, y, copy = copy)
  na_matches <- check_na_matches(na_matches)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  rows <- join_rows(x_key, y_key, type = "full")

  x_out <- set_names(x[vars$x$out], names(vars$x$out))
  y_out <- set_names(y[vars$y$out], names(vars$y$out))

  out <- vec_slice(x_out, c(rows$x, rep_along(rows$y_extra, NA_integer_)))
  out[names(x_key)] <- vec_rbind(
    vec_slice(x_key, rows$x),
    vec_slice(y_key, rows$y_extra)
  )
  out[names(y_out)] <- vec_slice(y_out, c(rows$y, rows$y_extra))
  out
}

#' @export
#' @rdname join.tbl_df
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by)
  y <- auto_copy(x, y, copy = copy)
  na_matches <- check_na_matches(na_matches)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  indx <- which(vec_in(x_key, y_key))
  x[indx, , drop = FALSE]
}

#' @export
#' @rdname join.tbl_df
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by)
  y <- auto_copy(x, y, copy = copy)
  na_matches <- check_na_matches(na_matches)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  indx <- which(!vec_in(x_key, y_key))
  x[indx, , drop = FALSE]
}

#' @export
#' @rdname join.tbl_df
nest_join.data.frame <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  name_var <- name %||% as_label(enexpr(y))
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by, suffix = c(".x", ".y"))
  y <- auto_copy(x, y, copy = copy)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  y_split <- vec_group_pos(y_key)
  matches <- vec_match(x_key, y_split$key)
  y_loc <- y_split$pos[matches]

  out <- set_names(x[vars$x$out], names(vars$x$out))
  out[names(x_key)] <- vec_cast(out[names(x_key)], vec_ptype2(x_key, y_key))

  y_out <- set_names(y[vars$y$out], names(vars$y$out))
  out[[name_var]] <- map(y_loc, vec_slice, x = y_out)
  out
}
