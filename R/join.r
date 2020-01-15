#' Mutating joins
#'
#' @description
#' The mutating joins add columns from `y` to `x`, matching rows based on the
#' keys:
#'
#' * `inner_join()`: includes all rows in `x` and `y`.
#' * `left_join()`: includes all rows in `x`.
#' * `right_join()`: includes all rows in `y`.
#' * `full_join()`: includes all rows in `x` or `y`.
#'
#' If there are multiple matches between `x` and `y`, all combination of the
#' matches are returned.
#'
#' @return
#' An object of the same type as `x`. The order of the rows and columns of `x`
#' is preserved as much as possible.
#'
#' * For `inner_join()`, a subset of the `x` rows.
#'   For `left_join()`, all `x` rows.
#'   For `right_join()`, a subset of `x` rows, followed by unmatched `y` rows.
#'   For `full_join()`, all `x` rows, followed by unmatched `y` rows.
#' * For all joins, rows will be duplicated if one row in `x` rows match
#'   multiple rows in `y`.
#' * Output columns include all `x` columns and all `y` columns. If the
#'   columns have the same name (and aren't included `y`), `suffix`es are
#'   added to disambiguate.
#' * Output columns columns included `by` are coerced to common type across
#'   `x` and `y`.
#' * Groups are taken from `x`.
#' @section Methods:
#' These function are **generic**s, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#'
#' * `inner_join()`: \Sexpr[stage=render,results=Rd]{dplyr:::methods_rd("inner_join")}.
#' * `left_join()`: \Sexpr[stage=render,results=Rd]{dplyr:::methods_rd("left_join")}.
#' * `right_join()`: \Sexpr[stage=render,results=Rd]{dplyr:::methods_rd("right_join")}.
#' * `full_join()`: \Sexpr[stage=render,results=Rd]{dplyr:::methods_rd("full_join")}.
#' @param x,y A pair of data frames, data frame extensions (e.g. a tibble), or
#'   lazy data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
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
#' @param keep Should the join keys from `y` be preserved in the output?
#'    Only applies to `nest_join()` and `full_join()`.
#' @param ... Other parameters passed onto methods.
#'
#'   For example, `na_matches` controls how `NA` values are handled when
#'   joining data frames. See [join.data.frame] for details.
#' @family joins
#' @examples
#' band_members %>% inner_join(band_instruments)
#' band_members %>% left_join(band_instruments)
#' band_members %>% right_join(band_instruments)
#' band_members %>% full_join(band_instruments)
#'
#' # To suppress the message about joining variables, supply `by`
#' band_members %>% inner_join(band_instruments, by = "name")
#' # This is good practice in production code
#'
#' # Use a named `by` if the join variables have different names
#' band_members %>% full_join(band_instruments2, by = c("name" = "artist"))
#' # By default only the join keys from `x` are kept; use `keep = TRUE`
#' # to keep both
#' band_members %>%
#'   full_join(band_instruments2, by = c("name" = "artist"), keep = TRUE)
#'
#' # Note that if a row in `x` matches multiple rows in `y`, all rows
#' # will be returned
#' df1 <- tibble(x = 1:3)
#' df2 <- tibble(x = c(1, 1, 2), y = c("first", "second", "third"))
#' df1 %>% left_join(df2)
#' @aliases join
#' @name mutate-joins
NULL

#' @rdname mutate-joins
#' @export
inner_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("inner_join")
}

#' @rdname mutate-joins
#' @export
left_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("left_join")
}

#' @rdname mutate-joins
#' @export
right_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  UseMethod("right_join")
}

#' @rdname mutate-joins
#' @export
full_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE) {
  UseMethod("full_join")
}

#' Filtering joins
#'
#' @description
#' Filtering joins filter rows from `x` based on the presence or absence
#' of matches in `y`:
#'
#' * `semi_join()` return all rows from `x` with a match in `y`.
#' * `anti_join()` return all rows from `x` with**out** a match in `y`.
#'
#' @param x,y A pair of data frames, data frame extensions (e.g. a tibble), or
#'   lazy data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
#' @inheritParams left_join
#' @return
#' An object of the same type as `x`.
#'
#' * Rows are a subset of the input, but appear in the same order.
#' * Columns are not modified.
#' * Data frame attributes are preserved.
#' * Groups are taken from `x`. The number of groups may be reduced.
#' @section Methods:
#' These function are **generic**s, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#'
#' * `semi_join()`: \Sexpr[stage=render,results=Rd]{dplyr:::methods_rd("semi_join")}.
#' * `anti_join()`: \Sexpr[stage=render,results=Rd]{dplyr:::methods_rd("anti_join")}.
#' @family joins
#' @examples
#' # "Filtering" joins keep cases from the LHS
#' band_members %>% semi_join(band_instruments)
#' band_members %>% anti_join(band_instruments)
#'
#' # To suppress the message about joining variables, supply `by`
#' band_members %>% semi_join(band_instruments, by = "name")
#' # This is good practice in production code
#' @name filter-joins
NULL

#' @rdname filter-joins
#' @export
semi_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("semi_join")
}

#' @rdname filter-joins
#' @export
anti_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("anti_join")
}

#' Nest join
#'
#' `nest_join()` returns all rows and columns `x` with a new nested-df column
#' that contains all matches from `y`. When there is no match, the list column
#' is a 0-row tibble.
#'
#' In some sense, a `nest_join()` is the most fundamental join since you can
#' recreate the other joins from it:
#'
#' * `inner_join()` is a `nest_join()` plus [tidyr::unnest()]
#' * `left_join()` `nest_join()` plus `unnest(.drop = FALSE)`.
#' * `semi_join()` is a `nest_join()` plus a `filter()` where you check
#'   that every element of data has at least one row,
#' * `anti_join()` is a `nest_join()` plus a `filter()` where you check every
#'   element has zero rows.
#'
#' @param x,y A pair of data frames, data frame extensions (e.g. a tibble), or
#'   lazy data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
#' @param name The name of the list column nesting joins create.
#'   If `NULL` the name of `y` is used.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=Rd]{dplyr:::methods_rd("nest_join")}.
#' @inheritParams left_join
#' @family joins
#' @export
#' @examples
#' band_members %>% nest_join(band_instruments)
nest_join <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  UseMethod("nest_join")
}

#' Join methods for data frames
#'
#' This page describes the details of the \link{join} generics when applied to
#' data frames and tibbles.
#'
#' @inheritParams inner_join
#' @inheritParams nest_join
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

  y_split <- vec_group_loc(y_key)
  matches <- vec_match(x_key, y_split$key)
  y_loc <- y_split$loc[matches]

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
