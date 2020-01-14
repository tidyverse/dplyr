#' Join two data frames together
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
#' @param x,y Data frames
#' @param by A character vector of variables to join by.
#'
#'   If `NULL`, the default, `*_join()` will perofrm a natural join, using all
#'   variables in across `x` and `y`. A message lists the variables so that you
#'   can check they're correct; suppress the message by supply `by` explicitly.
#'
#'   To join by different variables on `x` and `y` use a named vector.
#'   For example, `by = c("a" = "b")` will match `x$a` to `y$b`.
#'
#'   To perform a cross-join, generating all combinations of `x` and `y`,
#'   use `by = character()`.
#' @param copy If `x` and `y` are not from the same data source,
#'   and `copy` is `TRUE`, then `y` will be copied into the
#'   same src as `x`.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param suffix If there are non-joined duplicate variables in `x` and
#'   `y`, these suffixes will be added to the output to disambiguate them.
#'   Should be a character vector of length 2.
#' @param name The name of the list column nesting joins create.
#'   If `NULL` the name of `y` is used.
#' @param keep Should the join keys from `y` be preserved in the output?
#'    Only applies to `nest_join()` and `full_join()`.
#' @param ... Other parameters passed onto methods.
#'
#'   For example, `na_matches` controls how `NA` values are handled when
#'   joining data frames. See [join.data.frame] for details.
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


#' Join methods for data frames
#'
#' @description
#' This page describes the details of the [join] generics when applied to
#' data frames and tibbles.
#'
#' @return
#' An object of the same type as `x`. The order of the rows and columns is
#' preserved as much as possible.
#'
#' For the mutating joins:
#'
#' * For `inner_join()`, a subset of the `x` rows`
#'   For `left_join()`, all `x` rows.
#'   For `right_join()`, a subset of `x` rows, followed by unmatch `y` rows.
#'   For `full_join()`, all `x` rows, followed by unmatched `y` rows.
#' * For all joins, rows will be duplicated if one row in `x` rows match
#'   multiple rows in `y`.
#' * Output columns include all `x` columns and all `y` columns. If the
#'   columns have the same name (and aren't included `y`), `suffix`es are
#'   added to disambiguate.
#' * Output columns columns included `by` are coerced to common type across
#'   `x` and `y`.
#' * Groups are taken from `x`.
#'
#' For the filtering joins:
#'
#' * Rows are a subset of the input, but appear in the same order.
#' * Columns are not modified.
#' * Data frame attributes are preserved.
#' * Groups are taken from `x`.
#' @inheritParams inner_join
#' @param x,y Data frames
#' @param ... Included for compatibility with the generic; otherwise ignored.
#' @param na_matches Should `NA` and `NaN` values match one another?
#'
#'   Use `"never"` to always treat two `NA` or `NaN` values as different, like
#'   joins for database sources, similarly to `merge(incomparables = FALSE)`.
#'   The default, `"na"`, always treats two `NA` or `NaN` values as equal,
#'   like `%in%`, [match()], [merge()].
#'
#'   Users and package authors can change the default behavior by calling
#'   `pkgconfig::set_config("dplyr::na_matches" = "never")`.
#' @examples
#' df1 <- data.frame(x = c(1, NA), y = 2)
#' df2 <- data.frame(x = c(1, NA), z = 3)
#'
#' # By default, NAs match other NAs so that there are two
#' # rows in the output:
#' left_join(df1, df2)
#'
#' # You can optionally request that NAs don't match, giving a
#' # a result that more closely resembles SQL joins
#' left_join(df1, df2, na_matches = "never")
#' @name join.data.frame
NULL

#' @export
#' @rdname join.data.frame
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {

  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y, by = by, type = "inner", suffix = suffix, na_matches = na_matches)
}

#' @export
#' @rdname join.data.frame
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y, by = by, type = "left", suffix = suffix, na_matches = na_matches)
}

#' @export
#' @rdname join.data.frame
right_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y, by = by, type = "right", suffix = suffix, na_matches = na_matches)
}

#' @export
#' @rdname join.data.frame
full_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             keep = FALSE,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {

  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y, by = by, type = "full", suffix = suffix, na_matches = na_matches, keep = keep)
}

#' @export
#' @rdname join.data.frame
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {

  y <- auto_copy(x, y, copy = copy)
  join_filter(x, y, by = by, type = "semi", na_matches = na_matches)
}

#' @export
#' @rdname join.data.frame
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {

  y <- auto_copy(x, y, copy = copy)
  join_filter(x, y, by = by, type = "anti", na_matches = na_matches)
}

#' @export
#' @rdname join.data.frame
nest_join.data.frame <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  name_var <- name %||% as_label(enexpr(y))
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by, suffix = c("", ""), keep_y = keep)
  y <- auto_copy(x, y, copy = copy)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  y_split <- vec_group_pos(y_key)
  matches <- vec_match(x_key, y_split$key)
  y_loc <- y_split$pos[matches]

  out <- set_names(x[vars$x$out], names(vars$x$out))

  # Modify all columns in one step so that we only need to re-group once
  # Currently, this regroups too often, because it looks like we're always
  # changing the key vars because of the cast
  new_cols <- vec_cast(out[names(x_key)], vec_ptype2(x_key, y_key))
  names(new_cols) <- x_key

  y_out <- set_names(y[vars$y$out], names(vars$y$out))
  new_cols[[name_var]] <- map(y_loc, vec_slice, x = y_out)

  dplyr_col_modify(out, new_cols)
}

# helpers -----------------------------------------------------------------

join_mutate <- function(x, y, by, type,
                        suffix = c(".x", ".y"),
                        na_matches = "na",
                        keep = FALSE
                        ) {
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by, suffix = suffix, keep_y = keep)
  na_matches <- check_na_matches(na_matches %||% "na")

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))
  rows <- join_rows(x_key, y_key, type = type)

  x_out <- set_names(x[vars$x$out], names(vars$x$out))
  y_out <- set_names(y[vars$y$out], names(vars$y$out))

  out <- as_tibble(x_out)
  out <- vec_slice(out, c(rows$x, rep_along(rows$y_extra, NA_integer_)))
  out[names(x_key)] <- vec_cast(out[names(x_key)], vec_ptype2(x_key, y_key))

  # If we're not keeping all y keys, need to copy over for the new rows
  if (!keep) {
    new_rows <- length(rows$x) + seq_along(rows$y_extra)
    out[new_rows, names(y_key)] <- vec_slice(y_key, rows$y_extra)
  }

  out[names(y_out)] <- vec_slice(y_out, c(rows$y, rows$y_extra))
  dplyr_reconstruct(out, x_out)
}

join_filter <- function(x, y, by = NULL, type, na_matches = "na") {
  vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by)
  na_matches <- check_na_matches(na_matches %||% "na")

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  idx <- switch(type,
    semi = vec_in(x_key, y_key),
    anti = !vec_in(x_key, y_key)
  )
  dplyr_row_slice(x, idx)
}

check_na_matches <- function(na_matches = c("na", "never")) {
  na_matches <- arg_match(na_matches)

  if (na_matches == "never") {
    warn("`na_matches = 'never' currently unsupported")
  }

  (na_matches == "na")
}
