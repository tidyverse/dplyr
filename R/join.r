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

check_valid_names <- function(names, warn_only = FALSE) {
  which_na <- which(is.na(names))
  alert <- if (warn_only) warn else abort

  if (length(which_na)) {
    alert(glue("Column `{cols}` cannot have NA as name",
      cols = glue_collapse(which_na, sep = ", ")
    ))
  }

  if (any(dup <- duplicated(names))){
    alert(glue("Column `{cols}` must have a unique name",
      cols = names[dup]
    ))
  }
}



#' @export
#' @rdname join.tbl_df
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {

  vars <- join_vars2(tbl_vars(x), tbl_vars(y), by = by, suffix = suffix)
  y <- auto_copy(x, y, copy = copy)
  na_matches <- check_na_matches(na_matches)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  y_split <- vec_group_pos(y_key)
  matches <- vec_match(x_key, y_split$key)

  # expand indices
  x_loc <- seq_len(nrow(x))[!is.na(matches)]
  y_loc <- y_split$pos[matches[!is.na(matches)]]
  x_loc <- rep(x_loc, lengths(y_loc))
  y_loc <- vec_c(!!!y_loc, .pytype = integer())

  x_out <- set_names(x[vars$x$out], names(vars$x$out))
  y_out <- set_names(y[vars$y$out], names(vars$y$out))

  out <- vec_slice(x_out, x_loc)
  out[names(x_key)] <- vec_cast(out[names(x_key)], vec_ptype2(x_key, y_key))
  out[names(y_out)] <- vec_slice(y_out, y_loc)
  out
}

#' @importFrom tibble add_column
#' @export
#' @rdname join.tbl_df
nest_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  name_var <- name %||% as_label(enexpr(y))

  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by)
  by_x <- check_by_x(vars$idx$x$by)
  by_names <- vars$alias[seq_len(length(by_x))]
  by_y <- vars$idx$y$by
  aux_y <- vars$idx$y$aux
  aux_x <- vars$idx$x$aux

  if (keep) {
    aux_y <- c(by_y, aux_y)
  }

  y_split <- vec_group_pos(set_names(y[, by_y, drop = FALSE], by_names))

  matches <- vec_match(
    set_names(x[, by_x, drop = FALSE], by_names),
    y_split$key
  )

  # expand indices
  y_indices <- y_split$pos

  # joined columns, cast to their common types
  joined <- x[, by_x, drop = FALSE]
  joined <- set_names(joined, vars$alias[seq_len(ncol(joined))])
  joined[] <- map2(joined, y[, by_y, drop = FALSE], function(joined_i, y_i) {
    vec_cast(joined_i, to = vec_ptype_common(joined_i, y_i))
  })

  # colums from x (no casting needed)
  x_result <- set_names(
    x[, aux_x, drop = FALSE],
    vars$alias[seq2(ncol(joined) + 1, ncol(x))]
  )

  # columns from y
  y_keep <- if (keep) y else y[, aux_y, drop = FALSE]
  y_result_list <- map(matches, function(idx) {
    if (identical(idx, NA_integer_)) {
      vec_slice(y_keep, 0L)
    } else {
      vec_slice(y_keep, y_indices[[idx]])
    }
  })

  out <- add_column(joined, !!!x_result, !!name_var := y_result_list)
  reconstruct_join(out, x, vars)
}

check_by_x <- function(by_x) {
  if (length(by_x) == 0L) {
    abort(
      "`by` must specify variables to join by",
      "dplyr_join_empty_by"
    )
  }
  by_x
}

#' @export
#' @rdname join.tbl_df
left_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- check_by_x(vars$idx$x$by)

  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  # unique values and where they are in each
  y_split <- vec_group_pos(y[, by_y, drop = FALSE])

  # matching uniques in x with uniques in y
  matches <- vec_match(x[, by_x, drop = FALSE], set_names(y_split$key, names(x)[by_x]))

  # for each unique value in x, expand the ids according to the number
  # of matches in y
  x_indices <- vec_c(!!!map2(matches, seq_along(matches), function(match, ids, rhs_id) {
    if (is.na(match)) {
      ids
    } else {
      vec_repeat(ids, each = length(rhs_id[[match]]))
    }
  }, rhs_id = y_split$pos), .ptype = integer())
  x_slice <- vec_slice(x, x_indices)

  # same for ids of y
  y_indices <- vec_c(!!!map2(matches, seq_along(matches), function(match, ids, rhs_id) {
    if (is.na(match)) {
      NA_integer_
    } else {
      rhs_id[[match]]
    }
  }, rhs_id = y_split$pos), .ptype = integer())
  y_slice <- vec_slice(y, y_indices)

  out <- new_list(ncol(x) + length(aux_y), names = vars$alias)

  # join columns, perhaps with casting,
  # x columns stay in same position
  join_ptype <- vec_ptype2(x[, by_x, drop = FALSE], set_names(y[, by_y, drop = FALSE], names(x)[by_x]))
  out[by_x] <- vec_cast(x_slice[, by_x, drop = FALSE], to = join_ptype)

  # other columns from x
  out[aux_x] <- x_slice[, aux_x, drop = FALSE]

  # then columns from y
  out[ncol(x) + seq_along(aux_y)] <- y_slice[, aux_y, drop = FALSE]

  reconstruct_join(as_tibble(out), x, vars)
}

#' @export
#' @rdname join.tbl_df
right_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- check_by_x(vars$idx$x$by)
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux
  alias <- vars$alias

  # unique values and where they are in each
  x_split <- vec_group_pos(x[, by_x, drop = FALSE])

  # matching uniques in x with uniques in y
  matches <- vec_match(
    y[, by_y, drop = FALSE],
    set_names(x_split$key, names(y)[by_y])
  )

  # for each unique value in y, expand the ids according to the number
  # of matches in x
  y_indices <- vec_c(!!!map2(matches, seq_along(matches), function(match, id, lhs_id) {
    if (is.na(match)) {
      id
    } else {
      vec_repeat(id, each = length(lhs_id[[match]]))
    }
  }, lhs_id = x_split$pos), .ptype = integer())

  # same for ids of x
  x_indices <- vec_c(!!!map2(matches, seq_along(matches), function(match, id, lhs_id) {
    if (is.na(match)) {
      NA_integer_
    } else {
      vec_repeat(lhs_id[[match]], times = length(id))
    }
  }, lhs_id = x_split$pos), .ptype = integer())

  x_slice <- vec_slice(x, x_indices)
  y_slice <- vec_slice(y, y_indices)

  out <- new_list(ncol(x) + length(aux_y), names = vars$alias)

  # the joined columns (taken from `y`) and then cast to common type
  join_ptype <- vec_ptype2(x[, by_x, drop = FALSE], set_names(y[, by_y, drop = FALSE], names(x)[by_x]))
  out[by_x] <- vec_cast(set_names(y_slice[, by_y, drop = FALSE], names(x)[by_x]), to = join_ptype)

  # other colums from x
  out[aux_x] <- x_slice[, aux_x, drop = FALSE]

  # then columns from y
  out[ncol(x) + seq_along(aux_y)] <- y_slice[, aux_y, drop = FALSE]

  reconstruct_join(as_tibble(out), x, vars)
}

#' @export
#' @rdname join.tbl_df
full_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- check_by_x(vars$idx$x$by)
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux
  by_names <- vars$alias[seq_len(length(by_x))]

  # unique values and where they are in each
  x_split <- vec_group_pos(set_names(x[, by_x, drop = FALSE], by_names))
  y_split <- vec_group_pos(set_names(y[, by_y, drop = FALSE], by_names))

  # matching uniques in x with uniques in y and vice versa
  x_matches <- vec_match(x_split$key, y_split$key)
  y_matches <- vec_match(y_split$key, x_split$key)

  # expand x indices from x matches
  x_indices_one <- vec_c(
    !!!map2(x_matches, x_split$pos, function(match, ids, rhs_id) {
      if (is.na(match)) {
        ids
      } else {
        vec_repeat(ids, each = length(rhs_id[[match]]))
      }
    }, rhs_id = y_split$pos),
    .ptype = integer()
  )

  x_indices_two <- rep(NA_integer_,
    sum(lengths(y_split$pos[is.na(y_matches)]))
  )

  # rows in x
  y_indices_one <- vec_c(
    !!!map2(x_matches, x_split$pos, function(match, ids, rhs_id) {
      if (is.na(match)) {
        vec_repeat(NA_integer_, length(ids))
      } else {
        vec_repeat(rhs_id[[match]], times = length(ids))
      }
    }, rhs_id = y_split$pos),

    .ptype = integer()
  )

  # rows in y and not in x
  y_indices_two <- vec_c(!!!y_split$pos[is.na(y_matches)], .ptype = integer())

  out <- new_list(ncol(x) + length(aux_y), names = vars$alias)

  out[by_x] <- vec_rbind(
    vec_slice(x[, by_x, drop = FALSE], x_indices_one),
    set_names(vec_slice(y[, by_y, drop = FALSE], y_indices_two), names(x)[by_x])
  )

  # other colums from x
  out[aux_x] <- vec_slice(x[, aux_x, drop = FALSE], c(x_indices_one, x_indices_two))

  # columns from y
  out[ncol(x) + seq_along(aux_y)] <- vec_slice(y[, aux_y, drop = FALSE], c(y_indices_one, y_indices_two))

  reconstruct_join(as_tibble(out), x, vars)
}

#' @export
#' @rdname join.tbl_df
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))

  vars <- join_vars2(tbl_vars(x), tbl_vars(y), by = by)
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
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))

  vars <- join_vars2(tbl_vars(x), tbl_vars(y), by = by)
  y <- auto_copy(x, y, copy = copy)
  na_matches <- check_na_matches(na_matches)

  x_key <- set_names(x[vars$x$key], names(vars$x$key))
  y_key <- set_names(y[vars$y$key], names(vars$y$key))

  indx <- which(!vec_in(x_key, y_key))
  x[indx, , drop = FALSE]
}

reconstruct_join <- function(out, x, vars) {
  if (is_grouped_df(x)) {
    groups_in_old <- match(group_vars(x), tbl_vars(x))
    groups_in_alias <- match(groups_in_old, vars$x)
    out <- grouped_df(out, vars$alias[groups_in_alias], group_by_drop_default(x))
  }
  out
}

#' @export
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(left_join(as_tibble(x), y, by = by, copy = copy, ...))
}

#' @export
#' @rdname join.tbl_df
nest_join.data.frame <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ... ) {
  as.data.frame(nest_join(as_tibble(x), y, by = by, copy = copy, ..., keep = keep, name = name))
}

#' @export
right_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(right_join(as_tibble(x), y, by = by, copy = copy, ...))
}

#' @export
full_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(full_join(as_tibble(x), y, by = by, copy = copy, ...))
}

# Helpers -----------------------------------------------------------------

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
