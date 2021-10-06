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
#' If a row in `x` matches multiple rows in `y`, all the rows in `y` will be returned
#' once for each matching row in `x`.
#'
#' @return
#' An object of the same type as `x`. The order of the rows and columns of `x`
#' is preserved as much as possible. The output has the following properties:
#'
#' * For `inner_join()`, a subset of `x` rows.
#'   For `left_join()`, all `x` rows.
#'   For `right_join()`, a subset of `x` rows, followed by unmatched `y` rows.
#'   For `full_join()`, all `x` rows, followed by unmatched `y` rows.
#' * For all joins, rows will be duplicated if one or more rows in `x` matches
#'   multiple rows in `y`.
#' * Output columns include all `x` columns and all `y` columns. If columns in
#'   `x` and `y` have the same name (and aren't included in `by`), `suffix`es are
#'   added to disambiguate.
#' * Output columns included in `by` are coerced to common type across
#'   `x` and `y`.
#' * Groups are taken from `x`.
#' @section Methods:
#' These functions are **generic**s, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#'
#' * `inner_join()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("inner_join")}.
#' * `left_join()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("left_join")}.
#' * `right_join()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("right_join")}.
#' * `full_join()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("full_join")}.
#' @param x,y A pair of data frames, data frame extensions (e.g. a tibble), or
#'   lazy data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
#' @param by A character vector of variables to join by.
#'
#'   If `NULL`, the default, `*_join()` will perform a natural join, using all
#'   variables in common across `x` and `y`. A message lists the variables so that you
#'   can check they're correct; suppress the message by supplying `by` explicitly.
#'
#'   To join by different variables on `x` and `y`, use a named vector.
#'   For example, `by = c("a" = "b")` will match `x$a` to `y$b`.
#'
#'   To join by multiple variables, use a vector with length > 1.
#'   For example, `by = c("a", "b")` will match `x$a` to `y$a` and `x$b` to
#'   `y$b`. Use a named vector to match different variables in `x` and `y`.
#'   For example, `by = c("a" = "b", "c" = "d")` will match `x$a` to `y$b` and
#'   `x$c` to `y$d`.
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
#' @param keep Should the join keys from both `x` and `y` be preserved in the
#'   output?
#'   - If `NULL`, the default, equi join conditions retain only the keys from
#'     `x`, while non-equi join conditions retain the keys from both inputs.
#'   - If `TRUE`, keys from both inputs are retained.
#'   - If `FALSE`, only keys from `x` are retained. For right and full joins,
#'     the data in key columns corresponding to rows that only exist in `y` are
#'     merged into the key columns from `x`.
#' @param ... Other parameters passed onto methods.
#' @param na_matches Should `NA` and `NaN` values match one another?
#'
#'   The default, `"na"`, treats two `NA` or `NaN` values as equal, like
#'   `%in%`, [match()], [merge()].
#'
#'   Use `"never"` to always treat two `NA` or `NaN` values as different, like
#'   joins for database sources, similarly to `merge(incomparables = FALSE)`.
#' @param multiple Handling of rows in `x` with multiple matches in `y`.
#'   For each row of `x`:
#'   - `"all"` returns every match detected in `y`.
#'   - `"first"` returns the first match detected in `y`.
#'   - `"last"` returns the last match detected in `y`.
#'   - `"warning"` throws a warning if multiple matches are detected, but
#'     otherwise falls back to `"all"`.
#'   - `"error"` throws an error if multiple matches are detected.
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
#' # By default, the join keys from `x` and `y` are coalesced in the output; use
#' # `keep = TRUE` to keep the join keys from both `x` and `y`
#' band_members %>%
#'   full_join(band_instruments2, by = c("name" = "artist"), keep = TRUE)
#'
#' # If a row in `x` matches multiple rows in `y`, all the rows in `y` will be
#' # returned once for each matching row in `x`
#' df1 <- tibble(x = 1:3)
#' df2 <- tibble(x = c(1, 1, 2), y = c("first", "second", "third"))
#' df1 %>% left_join(df2)
#'
#' # By default, NAs match other NAs so that there are two
#' # rows in the output of this join:
#' df1 <- data.frame(x = c(1, NA), y = 2)
#' df2 <- data.frame(x = c(1, NA), z = 3)
#' left_join(df1, df2)
#'
#' # You can optionally request that NAs don't match, giving a
#' # a result that more closely resembles SQL joins
#' left_join(df1, df2, na_matches = "never")
#' @aliases join join.data.frame
#' @name mutate-joins
NULL

#' @export
#' @rdname mutate-joins
inner_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) {
  UseMethod("inner_join")
}

#' @export
#' @rdname mutate-joins
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ...,
                                  keep = NULL,
                                  na_matches = c("na", "never"),
                                  multiple = "all") {

  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y, by = by, type = "inner", suffix = suffix, na_matches = na_matches, keep = keep, multiple = multiple)
}

#' @export
#' @rdname mutate-joins
left_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) {
  UseMethod("left_join")
}

#' @export
#' @rdname mutate-joins
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             keep = NULL,
                             na_matches = c("na", "never"),
                             multiple = "all") {
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y, by = by, type = "left", suffix = suffix, na_matches = na_matches, keep = keep, multiple = multiple)
}

#' @export
#' @rdname mutate-joins
right_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) {
  UseMethod("right_join")
}

#' @export
#' @rdname mutate-joins
right_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              keep = NULL,
                              na_matches = c("na", "never"),
                              multiple = "all") {
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y, by = by, type = "right", suffix = suffix, na_matches = na_matches, keep = keep, multiple = multiple)
}

#' @export
#' @rdname mutate-joins
full_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) {
  UseMethod("full_join")
}

#' @export
#' @rdname mutate-joins
full_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             keep = NULL,
                             na_matches = c("na", "never"),
                             multiple = "all") {

  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y, by = by, type = "full", suffix = suffix, na_matches = na_matches, keep = keep, multiple = multiple)
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
#' An object of the same type as `x`. The output has the following properties:
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
#' * `semi_join()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("semi_join")}.
#' * `anti_join()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("anti_join")}.
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

#' @export
#' @rdname filter-joins
semi_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("semi_join")
}

#' @export
#' @rdname filter-joins
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = c("na", "never")) {

  y <- auto_copy(x, y, copy = copy)
  join_filter(x, y, by = by, type = "semi", na_matches = na_matches)
}

#' @export
#' @rdname filter-joins
anti_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("anti_join")
}

#' @export
#' @rdname filter-joins
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...,
                                 na_matches = c("na", "never")) {

  y <- auto_copy(x, y, copy = copy)
  join_filter(x, y, by = by, type = "anti", na_matches = na_matches)
}

#' Nest join
#'
#' `nest_join()` returns all rows and columns in `x` with a new nested-df column
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
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("nest_join")}.
#' @inheritParams left_join
#' @family joins
#' @export
#' @examples
#' band_members %>% nest_join(band_instruments)
nest_join <- function(x, y, by = NULL, copy = FALSE, keep = NULL, name = NULL, ...) {
  UseMethod("nest_join")
}

#' @export
#' @rdname nest_join
nest_join.data.frame <- function(x, y, by = NULL, copy = FALSE, keep = NULL, name = NULL, ...) {
  name_var <- name %||% as_label(enexpr(y))

  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  by <- standardise_join_by(by, x_names = x_names, y_names = y_names)

  vars <- join_cols(x_names, y_names, by = by, suffix = c("", ""), keep = keep)
  y <- auto_copy(x, y, copy = copy)

  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")

  x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
  y_key <- set_names(y_in[vars$y$key], names(vars$y$key))

  condition <- standardise_join_condition(by)
  filter <- by$filter

  matches <- vctrs:::vec_matches(
    needles = x_key,
    haystack = y_key,
    condition = condition,
    filter = filter,
    missing = "match",
    nan_distinct = TRUE,
    no_match = 0L
  )

  y_loc <- vec_split(matches$haystack, matches$needles)$val

  out <- set_names(x_in[vars$x$out], names(vars$x$out))

  # Modify all columns in one step so that we only need to re-group once
  # Currently, this regroups too often, because it looks like we're always
  # changing the key vars because of the cast
  new_cols <- vec_cast(out[names(x_key)], vec_ptype2(x_key, y_key))

  y_out <- set_names(y_in[vars$y$out], names(vars$y$out))
  new_cols[[name_var]] <- map(y_loc, vec_slice, x = y_out)

  out <- dplyr_col_modify(out, new_cols)
  dplyr_reconstruct(out, x)
}

# helpers -----------------------------------------------------------------

join_mutate <- function(x, y, by, type,
                        suffix = c(".x", ".y"),
                        na_matches = c("na", "never"),
                        keep = NULL,
                        multiple = "all") {
  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  by <- standardise_join_by(by, x_names = x_names, y_names = y_names)

  vars <- join_cols(x_names, y_names, by = by, suffix = suffix, keep = keep)
  missing <- check_na_matches(na_matches)

  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")

  x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
  y_key <- set_names(y_in[vars$y$key], names(vars$y$key))

  condition <- standardise_join_condition(by)
  filter <- by$filter

  rows <- join_rows(
    x_key = x_key,
    y_key = y_key,
    type = type,
    missing = missing,
    condition = condition,
    filter = filter,
    multiple = multiple
  )

  x_slicer <- rows$x
  y_slicer <- rows$y

  x_out <- set_names(x_in[vars$x$out], names(vars$x$out))
  y_out <- set_names(y_in[vars$y$out], names(vars$y$out))

  out <- vec_slice(x_out, x_slicer)
  out[names(y_out)] <- vec_slice(y_out, y_slicer)

  if (!is_true(keep)) {
    if (is_null(keep)) {
      merge <- by$x[by$condition == "=="]
    } else if (is_false(keep)) {
      merge <- by$x
    }

    x_merge <- x_key[merge]
    y_merge <- y_key[merge]

    key_type <- vec_ptype_common(x_merge, y_merge)
    out[names(x_merge)] <- vec_cast(out[names(x_merge)], key_type)

    if ((type == "right" || type == "full") && anyNA(x_slicer)) {
      new_rows <- which(is.na(x_slicer))
      y_replacer <- y_slicer[new_rows]
      out[new_rows, names(y_merge)] <- vec_cast(vec_slice(y_merge, y_replacer), key_type)
    }
  }

  dplyr_reconstruct(out, x)
}

join_filter <- function(x, y, by = NULL, type, na_matches = c("na", "never")) {
  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  by <- standardise_join_by(by, x_names = x_names, y_names = y_names)

  vars <- join_cols(x_names, y_names, by = by)
  missing <- check_na_matches(na_matches)

  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")

  x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
  y_key <- set_names(y_in[vars$y$key], names(vars$y$key))

  condition <- standardise_join_condition(by)
  filter <- by$filter

  # We only care about whether or not any matches exist
  multiple <- "first"

  if (type == "semi") {
    no_match <- "drop"

    if (missing == "propagate") {
      missing <- "drop"
    }
  } else {
    no_match <- NA_integer_
  }

  matches <- vctrs:::vec_matches(
    needles = x_key,
    haystack = y_key,
    condition = condition,
    filter = filter,
    missing = missing,
    no_match = no_match,
    nan_distinct = TRUE,
    multiple = multiple
  )

  if (type == "semi") {
    # Unmatched needles and propagated missing needles will already be dropped
    idx <- matches$needles
  } else {
    # Treat both unmatched needles and propagated missing needles as no-match
    unmatched <- is.na(matches$haystack)
    idx <- matches$needles[unmatched]
  }

  dplyr_row_slice(x, idx)
}

check_na_matches <- function(na_matches = c("na", "never")) {
  if (isNamespaceLoaded("pkgconfig")) {
    conf <- asNamespace("pkgconfig")$get_config("dplyr::na_matches")
    if (!is.null(conf)) {
      warn(c(
        "`dplyr::na_matches` pkgconfig options is now ignored.",
        "Please set `na_matches` directly."
      ))
    }
  }

  na_matches <- arg_match(na_matches)

  switch(
    na_matches,
    na = "match",
    never = "propagate"
  )
}

standardise_join_condition <- function(by) {
  condition <- by$condition

  if (identical(condition, character())) {
    # Cross join, `by = join_by()` or `by = character()`
    condition <- NULL
  }

  condition
}
