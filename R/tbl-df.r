#' Create a data frame tbl.
#'
#' Deprecated: please use [tibble::as_tibble()] instead.
#'
#' @export
#' @keywords internal
#' @param data a data frame
tbl_df <- function(data) {
  as_data_frame(data)
}

#' @export
as.tbl.data.frame <- function(x, ...) {
  tbl_df(x)
}

#' @export
tbl_vars.data.frame <- function(x) names(x)

#' @export
same_src.data.frame <- function(x, y) {
  is.data.frame(y)
}

#' @export
auto_copy.tbl_df <- function(x, y, copy = FALSE, ...) {
  as.data.frame(y)
}

# Grouping methods ------------------------------------------------------------

# These are all inherited from data.frame - see tbl-data-frame.R

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_df <- function(x, row.names = NULL, optional = FALSE, ...) {
  as_regular_df(x)
}

# Verbs ------------------------------------------------------------------------

#' @export
arrange.tbl_df <- function(.data, ...) {
  dots <- quos(...)
  if (any(names(dots) != "")) {
    names <- names(dots)
    bad_pos_args(names[names != ""], "must not be named")
  }
  arrange_impl(.data, dots)
}
#' @export
arrange_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  arrange_impl(.data, dots)
}

#' @export
filter.tbl_df <- function(.data, ..., .preserve = TRUE) {
  dots <- quos(...)
  if (any(have_name(dots))) {
    bad <- dots[have_name(dots)]
    bad_eq_ops(bad, "must not be named, do you need `==`?")
  } else if (is_empty(dots)) {
    return(.data)
  }

  quo <- all_exprs(!!!dots, .vectorised = TRUE)
  out <- filter_impl(.data, quo)
  if (!.preserve) {
    out <- group_by(out, add = TRUE)
  }
  out
}
#' @export
filter_.tbl_df <- function(.data, ..., .dots = list(), .preserve = TRUE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!!dots, .preserve = .preserve)
}

#' @export
slice.tbl_df <- function(.data, ...) {
  dots <- named_quos(...)
  slice_impl(.data, dots)
}
#' @export
slice_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  slice_impl(.data, dots)
}

#' @export
mutate.tbl_df <- function(.data, ...) {
  dots <- named_quos(...)
  mutate_impl(.data, dots)
}
#' @export
mutate_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  mutate_impl(.data, dots)
}

#' @export
summarise.tbl_df <- function(.data, ...) {
  dots <- named_quos(...)
  summarise_impl(.data, dots)
}
#' @export
summarise_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  summarise_impl(.data, dots)
}

# Joins ------------------------------------------------------------------------

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
#' batting_df <- tbl_df(Batting)
#' person_df <- tbl_df(Master)
#'
#' uperson_df <- tbl_df(Master[!duplicated(Master$playerID), ])
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
inner_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  out <- inner_join_impl(x, y, by_x, by_y, aux_x, aux_y, na_matches)
  names(out) <- vars$alias

  reconstruct_join(out, x, vars)
}

#' @export
#' @rdname join.tbl_df
nest_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, name = "data", ...) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by)
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_y <- vars$idx$y$aux

  out <- nest_join_impl(x, y, by_x, by_y, aux_y, name)
  out
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
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  out <- left_join_impl(x, y, by_x, by_y, aux_x, aux_y, na_matches)
  names(out) <- vars$alias

  reconstruct_join(out, x, vars)
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
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  out <- right_join_impl(x, y, by_x, by_y, aux_x, aux_y, na_matches)
  names(out) <- vars$alias

  reconstruct_join(out, x, vars)
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
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  out <- full_join_impl(x, y, by_x, by_y, aux_x, aux_y, na_matches)
  names(out) <- vars$alias

  reconstruct_join(out, x, vars)
}

#' @export
#' @rdname join.tbl_df
semi_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x), warn_only = TRUE)
  check_valid_names(tbl_vars(y), warn_only = TRUE)

  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  out <- semi_join_impl(x, y, by$x, by$y, check_na_matches(na_matches))
  if (is_grouped_df(x)) {
    out <- grouped_df_impl(out, group_vars(x))
  }
  out
}

#' @export
#' @rdname join.tbl_df
anti_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x), warn_only = TRUE)
  check_valid_names(tbl_vars(y), warn_only = TRUE)

  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  out <- anti_join_impl(x, y, by$x, by$y, check_na_matches(na_matches))
  if (is_grouped_df(x)) {
    out <- grouped_df_impl(out, group_vars(x))
  }
  out
}

reconstruct_join <- function(out, x, vars) {
  if (is_grouped_df(x)) {
    groups_in_old <- match(group_vars(x), tbl_vars(x))
    groups_in_alias <- match(groups_in_old, vars$x)
    out <- grouped_df_impl(out, vars$alias[groups_in_alias])
  }
  out
}


# Set operations ---------------------------------------------------------------

#' @export
# Can't use NextMethod() in R 3.1, r-lib/rlang#486
distinct.tbl_df <- distinct.data.frame
#' @export
# Can't use NextMethod() in R 3.1, r-lib/rlang#486
distinct_.tbl_df <- distinct_.data.frame
