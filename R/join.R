#' Mutating joins
#'
#' @description
#' Mutating joins add columns from `y` to `x`, matching observations based on
#' the keys. There are four mutating joins: the inner join, and the three outer
#' joins.
#'
#' ## Inner join
#'
#' An `inner_join()` only keeps observations from `x` that have a matching key
#' in `y`.
#'
#' The most important property of an inner join is that unmatched rows in either
#' input are not included in the result. This means that generally inner joins
#' are not appropriate in most analyses, because it is too easy to lose
#' observations.
#'
#' ## Outer joins
#'
#' The three outer joins keep observations that appear in at least one of the
#' data frames:
#'
#' * A `left_join()` keeps all observations in `x`.
#'
#' * A `right_join()` keeps all observations in `y`.
#'
#' * A `full_join()` keeps all observations in `x` and `y`.
#'
#' @section Many-to-many relationships:
#'
#' By default, dplyr guards against many-to-many relationships in equality joins
#' by throwing a warning. These occur when both of the following are true:
#'
#' - A row in `x` matches multiple rows in `y`.
#' - A row in `y` matches multiple rows in `x`.
#'
#' This is typically surprising, as most joins involve a relationship of
#' one-to-one, one-to-many, or many-to-one, and is often the result of an
#' improperly specified join. Many-to-many relationships are particularly
#' problematic because they can result in a Cartesian explosion of the number of
#' rows returned from the join.
#'
#' If a many-to-many relationship is expected, silence this warning by
#' explicitly setting `relationship = "many-to-many"`.
#'
#' In production code, it is best to preemptively set `relationship` to whatever
#' relationship you expect to exist between the keys of `x` and `y`, as this
#' forces an error to occur immediately if the data doesn't align with your
#' expectations.
#'
#' Inequality joins typically result in many-to-many relationships by nature, so
#' they don't warn on them by default, but you should still take extra care when
#' specifying an inequality join, because they also have the capability to
#' return a large number of rows.
#'
#' Rolling joins don't warn on many-to-many relationships either, but many
#' rolling joins follow a many-to-one relationship, so it is often useful to
#' set `relationship = "many-to-one"` to enforce this.
#'
#' Note that in SQL, most database providers won't let you specify a
#' many-to-many relationship between two tables, instead requiring that you
#' create a third _junction table_ that results in two one-to-many relationships
#' instead.
#'
#' @return
#' An object of the same type as `x` (including the same groups). The order of
#' the rows and columns of `x` is preserved as much as possible. The output has
#' the following properties:
#'
#' * The rows are affect by the join type.
#'    * `inner_join()` returns matched `x` rows.
#'    * `left_join()` returns all `x` rows.
#'    * `right_join()`  returns matched of `x` rows, followed by unmatched `y` rows.
#'    * `full_join()`  returns all `x` rows, followed by unmatched `y` rows.
#' * Output columns include all columns from `x` and all non-key columns from
#'   `y`. If `keep = TRUE`, the key columns from `y` are included as well.
#' * If non-key columns in `x` and `y` have the same name, `suffix`es are added
#'   to disambiguate. If `keep = TRUE` and key columns in `x` and `y` have
#'   the same name, `suffix`es are added to disambiguate these as well.
#' * If `keep = FALSE`, output columns included in `by` are coerced to their
#'   common type between `x` and `y`.
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
#' @param by A join specification created with [join_by()], or a character
#'   vector of variables to join by.
#'
#'   If `NULL`, the default, `*_join()` will perform a natural join, using all
#'   variables in common across `x` and `y`. A message lists the variables so
#'   that you can check they're correct; suppress the message by supplying `by`
#'   explicitly.
#'
#'   To join on different variables between `x` and `y`, use a [join_by()]
#'   specification. For example, `join_by(a == b)` will match `x$a` to `y$b`.
#'
#'   To join by multiple variables, use a [join_by()] specification with
#'   multiple expressions. For example, `join_by(a == b, c == d)` will match
#'   `x$a` to `y$b` and `x$c` to `y$d`. If the column names are the same between
#'   `x` and `y`, you can shorten this by listing only the variable names, like
#'   `join_by(a, c)`.
#'
#'   [join_by()] can also be used to perform inequality, rolling, and overlap
#'   joins. See the documentation at [?join_by][join_by()] for details on
#'   these types of joins.
#'
#'   For simple equality joins, you can alternatively specify a character vector
#'   of variable names to join by. For example, `by = c("a", "b")` joins `x$a`
#'   to `y$a` and `x$b` to `y$b`. If variable names differ between `x` and `y`,
#'   use a named character vector like `by = c("x_a" = "y_a", "x_b" = "y_b")`.
#'
#'   To perform a cross-join, generating all combinations of `x` and `y`, see
#'   [cross_join()].
#' @param copy If `x` and `y` are not from the same data source,
#'   and `copy` is `TRUE`, then `y` will be copied into the
#'   same src as `x`.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param suffix If there are non-joined duplicate variables in `x` and
#'   `y`, these suffixes will be added to the output to disambiguate them.
#'   Should be a character vector of length 2.
#' @param keep Should the join keys from both `x` and `y` be preserved in the
#'   output?
#'   - If `NULL`, the default, joins on equality retain only the keys from `x`,
#'     while joins on inequality retain the keys from both inputs.
#'   - If `TRUE`, all keys from both inputs are retained.
#'   - If `FALSE`, only keys from `x` are retained. For right and full joins,
#'     the data in key columns corresponding to rows that only exist in `y` are
#'     merged into the key columns from `x`. Can't be used when joining on
#'     inequality conditions.
#' @param ... Other parameters passed onto methods.
#' @param na_matches Should two `NA` or two `NaN` values match?
#'   - `"na"`, the default, treats two `NA` or two `NaN` values as equal, like
#'   `%in%`, [match()], and [merge()].
#'   - `"never"` treats two `NA` or two `NaN` values as different, and will
#'   never match them together or to any other values. This is similar to joins
#'   for database sources and to `base::merge(incomparables = NA)`.
#' @param multiple Handling of rows in `x` with multiple matches in `y`.
#'   For each row of `x`:
#'   - `"all"`, the default, returns every match detected in `y`. This is the
#'     same behavior as SQL.
#'   - `"any"` returns one match detected in `y`, with no guarantees on which
#'     match will be returned. It is often faster than `"first"` and `"last"`
#'     if you just need to detect if there is at least one match.
#'   - `"first"` returns the first match detected in `y`.
#'   - `"last"` returns the last match detected in `y`.
#' @param unmatched How should unmatched keys that would result in dropped rows
#'   be handled?
#'   - `"drop"` drops unmatched keys from the result.
#'   - `"error"` throws an error if unmatched keys are detected.
#'
#'   `unmatched` is intended to protect you from accidentally dropping rows
#'   during a join. It only checks for unmatched keys in the input that could
#'   potentially drop rows.
#'   - For left joins, it checks `y`.
#'   - For right joins, it checks `x`.
#'   - For inner joins, it checks both `x` and `y`. In this case, `unmatched` is
#'     also allowed to be a character vector of length 2 to specify the behavior
#'     for `x` and `y` independently.
#' @param relationship Handling of the expected relationship between the keys of
#'   `x` and `y`. If the expectations chosen from the list below are
#'   invalidated, an error is thrown.
#'
#'   - `NULL`, the default, doesn't expect there to be any relationship between
#'     `x` and `y`. However, for equality joins it will check for a many-to-many
#'     relationship (which is typically unexpected) and will warn if one occurs,
#'     encouraging you to either take a closer look at your inputs or make this
#'     relationship explicit by specifying `"many-to-many"`.
#'
#'     See the _Many-to-many relationships_ section for more details.
#'
#'   - `"one-to-one"` expects:
#'     - Each row in `x` matches at most 1 row in `y`.
#'     - Each row in `y` matches at most 1 row in `x`.
#'
#'   - `"one-to-many"` expects:
#'     - Each row in `y` matches at most 1 row in `x`.
#'
#'   - `"many-to-one"` expects:
#'     - Each row in `x` matches at most 1 row in `y`.
#'
#'   - `"many-to-many"` doesn't perform any relationship checks, but is provided
#'     to allow you to be explicit about this relationship if you know it
#'     exists.
#'
#'   `relationship` doesn't handle cases where there are zero matches. For that,
#'   see `unmatched`.
#' @family joins
#' @examples
#' band_members |> inner_join(band_instruments)
#' band_members |> left_join(band_instruments)
#' band_members |> right_join(band_instruments)
#' band_members |> full_join(band_instruments)
#'
#' # To suppress the message about joining variables, supply `by`
#' band_members |> inner_join(band_instruments, by = join_by(name))
#' # This is good practice in production code
#'
#' # Use an equality expression if the join variables have different names
#' band_members |> full_join(band_instruments2, by = join_by(name == artist))
#' # By default, the join keys from `x` and `y` are coalesced in the output; use
#' # `keep = TRUE` to keep the join keys from both `x` and `y`
#' band_members |>
#'   full_join(band_instruments2, by = join_by(name == artist), keep = TRUE)
#'
#' # If a row in `x` matches multiple rows in `y`, all the rows in `y` will be
#' # returned once for each matching row in `x`.
#' df1 <- tibble(x = 1:3)
#' df2 <- tibble(x = c(1, 1, 2), y = c("first", "second", "third"))
#' df1 |> left_join(df2)
#'
#' # If a row in `y` also matches multiple rows in `x`, this is known as a
#' # many-to-many relationship, which is typically a result of an improperly
#' # specified join or some kind of messy data. In this case, a warning is
#' # thrown by default:
#' df3 <- tibble(x = c(1, 1, 1, 3))
#' df3 |> left_join(df2)
#'
#' # In the rare case where a many-to-many relationship is expected, set
#' # `relationship = "many-to-many"` to silence this warning
#' df3 |> left_join(df2, relationship = "many-to-many")
#'
#' # Use `join_by()` with a condition other than `==` to perform an inequality
#' # join. Here we match on every instance where `df1$x > df2$x`.
#' df1 |> left_join(df2, join_by(x > x))
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
inner_join <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  UseMethod("inner_join")
}

#' @export
#' @rdname mutate-joins
inner_join.data.frame <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL
) {
  check_dots_empty0(...)
  y <- auto_copy(x, y, copy = copy)
  join_mutate(
    x = x,
    y = y,
    by = by,
    type = "inner",
    suffix = suffix,
    na_matches = na_matches,
    keep = keep,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    user_env = caller_env()
  )
}

#' @export
#' @rdname mutate-joins
left_join <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  UseMethod("left_join")
}

#' @export
#' @rdname mutate-joins
left_join.data.frame <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL
) {
  check_dots_empty0(...)
  y <- auto_copy(x, y, copy = copy)
  join_mutate(
    x = x,
    y = y,
    by = by,
    type = "left",
    suffix = suffix,
    na_matches = na_matches,
    keep = keep,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    user_env = caller_env()
  )
}

#' @export
#' @rdname mutate-joins
right_join <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  UseMethod("right_join")
}

#' @export
#' @rdname mutate-joins
right_join.data.frame <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL
) {
  check_dots_empty0(...)
  y <- auto_copy(x, y, copy = copy)
  join_mutate(
    x = x,
    y = y,
    by = by,
    type = "right",
    suffix = suffix,
    na_matches = na_matches,
    keep = keep,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    user_env = caller_env()
  )
}

#' @export
#' @rdname mutate-joins
full_join <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  UseMethod("full_join")
}

#' @export
#' @rdname mutate-joins
full_join.data.frame <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  relationship = NULL
) {
  check_dots_empty0(...)
  y <- auto_copy(x, y, copy = copy)
  join_mutate(
    x = x,
    y = y,
    by = by,
    type = "full",
    suffix = suffix,
    na_matches = na_matches,
    keep = keep,
    multiple = multiple,
    # All keys from both inputs are retained. Erroring never makes sense.
    unmatched = "drop",
    relationship = relationship,
    user_env = caller_env()
  )
}

#' Filtering joins
#'
#' @description
#' Filtering joins filter rows from `x` based on the presence or absence
#' of matches in `y`:
#'
#' * `semi_join()` returns all rows from `x` with a match in `y`.
#' * `anti_join()` returns all rows from `x` with**out** a match in `y`.
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
#' band_members |> semi_join(band_instruments)
#' band_members |> anti_join(band_instruments)
#'
#' # To suppress the message about joining variables, supply `by`
#' band_members |> semi_join(band_instruments, by = join_by(name))
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
semi_join.data.frame <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  ...,
  na_matches = c("na", "never")
) {
  check_dots_empty0(...)
  y <- auto_copy(x, y, copy = copy)
  join_filter(
    x,
    y,
    by = by,
    type = "semi",
    na_matches = na_matches,
    user_env = caller_env()
  )
}

#' @export
#' @rdname filter-joins
anti_join <- function(x, y, by = NULL, copy = FALSE, ...) {
  UseMethod("anti_join")
}

#' @export
#' @rdname filter-joins
anti_join.data.frame <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  ...,
  na_matches = c("na", "never")
) {
  check_dots_empty0(...)
  y <- auto_copy(x, y, copy = copy)
  join_filter(
    x,
    y,
    by = by,
    type = "anti",
    na_matches = na_matches,
    user_env = caller_env()
  )
}

#' Nest join
#'
#' A nest join leaves `x` almost unchanged, except that it adds a new
#' list-column, where each element contains the rows from `y` that match the
#' corresponding row in `x`.
#'
#' # Relationship to other joins
#'
#' You can recreate many other joins from the result of a nest join:
#'
#' * [inner_join()] is a `nest_join()` plus [tidyr::unnest()].
#' * [left_join()] is a `nest_join()` plus `tidyr::unnest(keep_empty = TRUE)`.
#' * [semi_join()] is a `nest_join()` plus a `filter()` where you check
#'   that every element of data has at least one row.
#' * [anti_join()] is a `nest_join()` plus a `filter()` where you check that every
#'   element has zero rows.
#'
#' @param name The name of the list-column created by the join. If `NULL`,
#'   the default, the name of `y` is used.
#' @param keep Should the new list-column contain join keys? The default
#'   will preserve the join keys for inequality joins.
#' @return
#' The output:
#' * Is same type as `x` (including having the same groups).
#' * Has exactly the same number of rows as `x`.
#' * Contains all the columns of `x` in the same order with the same values.
#'   They are only modified (slightly) if `keep = FALSE`, when columns listed
#'   in `by` will be coerced to their common type across `x` and `y`.
#' * Gains one new column called `{name}` on the far right, a list column
#'   containing data frames the same type as `y`.
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
#' df1 <- tibble(x = 1:3)
#' df2 <- tibble(x = c(2, 3, 3), y = c("a", "b", "c"))
#'
#' out <- nest_join(df1, df2)
#' out
#' out$df2
nest_join <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  keep = NULL,
  name = NULL,
  ...
) {
  UseMethod("nest_join")
}

#' @export
#' @rdname nest_join
nest_join.data.frame <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  keep = NULL,
  name = NULL,
  ...,
  na_matches = c("na", "never"),
  unmatched = "drop"
) {
  check_dots_empty0(...)
  check_keep(keep)
  na_matches <- check_na_matches(na_matches)

  if (is.null(name)) {
    name <- as_label(enexpr(y))
  } else {
    check_string(name)
  }

  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  if (is_cross_by(by)) {
    warn_join_cross_by()
    by <- new_join_by()
    cross <- TRUE
  } else {
    cross <- FALSE
  }

  if (is_null(by)) {
    by <- join_by_common(x_names, y_names)
  } else {
    by <- as_join_by(by)
  }

  vars <- join_cols(x_names, y_names, by = by, suffix = c("", ""), keep = keep)
  y <- auto_copy(x, y, copy = copy)

  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")

  x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
  y_key <- set_names(y_in[vars$y$key], names(vars$x$key))

  args <- join_cast_common(x_key, y_key, vars)
  x_key <- args$x
  y_key <- args$y

  condition <- by$condition
  filter <- by$filter

  # We always want to retain all of the matches. We never experience a Cartesian
  # explosion because `nrow(x) == nrow(out)` is an invariant of `nest_join()`,
  # and the whole point of `nest_join()` is to nest all of the matches for that
  # row of `x` (#6392).
  multiple <- "all"

  # Will be set to `"none"` in `join_rows()`. Because we can't have a Cartesian
  # explosion, we don't care about many-to-many relationships.
  relationship <- NULL

  rows <- join_rows(
    x_key = x_key,
    y_key = y_key,
    type = "nest",
    na_matches = na_matches,
    condition = condition,
    filter = filter,
    cross = cross,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    user_env = caller_env()
  )

  y_loc <- vec_split(rows$y, rows$x)$val

  out <- set_names(x_in[vars$x$out], names(vars$x$out))

  # Modify all columns in one step so that we only need to re-group once
  new_cols <- vec_cast(out[names(x_key)], x_key)

  y_out <- set_names(y_in[vars$y$out], names(vars$y$out))
  y_out <- map(y_loc, vec_slice, x = y_out)
  y_out <- map(y_out, dplyr_reconstruct, template = y)
  new_cols[[name]] <- y_out

  out <- dplyr_col_modify(out, new_cols)
  dplyr_reconstruct(out, x)
}

# helpers -----------------------------------------------------------------

join_mutate <- function(
  x,
  y,
  by,
  type,
  ...,
  suffix = c(".x", ".y"),
  na_matches = "na",
  keep = NULL,
  multiple = "all",
  unmatched = "drop",
  relationship = NULL,
  error_call = caller_env(),
  user_env = caller_env()
) {
  check_dots_empty0(...)

  na_matches <- check_na_matches(
    na_matches,
    error_call = error_call
  )

  check_keep(keep, error_call = error_call)

  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  if (is_cross_by(by)) {
    warn_join_cross_by(env = error_call, user_env = user_env)
    by <- new_join_by()
    cross <- TRUE
  } else {
    cross <- FALSE
  }

  if (is_null(by)) {
    by <- join_by_common(x_names, y_names, error_call = error_call)
  } else {
    by <- as_join_by(by, error_call = error_call)
  }

  vars <- join_cols(
    x_names = x_names,
    y_names = y_names,
    by = by,
    suffix = suffix,
    keep = keep,
    error_call = error_call
  )

  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")

  x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
  y_key <- set_names(y_in[vars$y$key], names(vars$x$key))

  args <- join_cast_common(x_key, y_key, vars, error_call = error_call)
  x_key <- args$x
  y_key <- args$y

  condition <- by$condition
  filter <- by$filter

  rows <- join_rows(
    x_key = x_key,
    y_key = y_key,
    type = type,
    na_matches = na_matches,
    condition = condition,
    filter = filter,
    cross = cross,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    error_call = error_call,
    user_env = user_env
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
      # Won't ever contain non-equi conditions
      merge <- by$x
    }

    # Keys have already been cast to the common type
    x_merge <- x_key[merge]

    out[merge] <- vec_cast(
      x = out[merge],
      to = x_merge,
      call = error_call
    )

    if ((type == "right" || type == "full") && anyNA(x_slicer)) {
      y_merge <- y_key[merge]

      new_rows <- which(is.na(x_slicer))
      y_replacer <- y_slicer[new_rows]

      out[new_rows, merge] <- vec_slice(y_merge, y_replacer)
    }
  }

  dplyr_reconstruct(out, x)
}

join_filter <- function(
  x,
  y,
  by,
  type,
  ...,
  na_matches = c("na", "never"),
  error_call = caller_env(),
  user_env = caller_env()
) {
  check_dots_empty0(...)

  na_matches <- check_na_matches(
    na_matches,
    error_call = error_call
  )

  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  if (is_cross_by(by)) {
    warn_join_cross_by(env = error_call, user_env = user_env)
    by <- new_join_by()
    cross <- TRUE
  } else {
    cross <- FALSE
  }

  if (is_null(by)) {
    by <- join_by_common(x_names, y_names, error_call = error_call)
  } else {
    by <- as_join_by(by, error_call = error_call)
  }

  vars <- join_cols(x_names, y_names, by = by, error_call = error_call)

  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")

  x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
  y_key <- set_names(y_in[vars$y$key], names(vars$x$key))

  args <- join_cast_common(x_key, y_key, vars, error_call = error_call)
  x_key <- args$x
  y_key <- args$y

  condition <- by$condition
  filter <- by$filter

  # We only care about whether or not any matches exist
  multiple <- "any"

  # Will be set to `"none"` in `join_rows()`. Because `multiple = "any"`, that
  # means many-to-many relationships aren't possible.
  relationship <- NULL

  # Since we are actually testing the presence of matches, it doesn't make
  # sense to ever error on unmatched values.
  unmatched <- "drop"

  rows <- join_rows(
    x_key = x_key,
    y_key = y_key,
    type = type,
    na_matches = na_matches,
    condition = condition,
    filter = filter,
    cross = cross,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    error_call = error_call,
    user_env = user_env
  )

  if (type == "semi") {
    # Unmatched needles and propagated missing needles will already be dropped
    idx <- rows$x
  } else {
    # Treat both unmatched needles and propagated missing needles as no-match
    no_match <- is.na(rows$y)
    idx <- rows$x[no_match]
  }

  dplyr_row_slice(x, idx)
}

check_na_matches <- function(na_matches, ..., error_call = caller_env()) {
  arg_match0(
    arg = na_matches,
    values = c("na", "never"),
    error_call = error_call
  )
}

check_keep <- function(keep, error_call = caller_env()) {
  if (!is_bool(keep) && !is.null(keep)) {
    abort(
      glue(
        "`keep` must be `TRUE`, `FALSE`, or `NULL`, not {obj_type_friendly(keep)}."
      ),
      call = error_call
    )
  }
}

is_cross_by <- function(x) {
  if (is_character(x, n = 0L)) {
    # `character()` or `named character()`
    return(TRUE)
  }

  if (
    is_list(x, n = 2L) &&
      is_character(x[["x"]], n = 0L) &&
      is_character(x[["y"]], n = 0L)
  ) {
    # `list(x = character(), y = character())`
    # (possibly with named empty character elements)
    return(TRUE)
  }

  FALSE
}

warn_join_cross_by <- function(env = caller_env(), user_env = caller_env(2)) {
  # Also remove `join_rows(cross =)` and `is_cross_by()` once we remove support
  # for this
  lifecycle::deprecate_warn(
    when = "1.1.0",
    what = I("Using `by = character()` to perform a cross join"),
    with = "cross_join()",
    env = env,
    user_env = user_env,
    id = "dplyr-by-for-cross-join"
  )
}
