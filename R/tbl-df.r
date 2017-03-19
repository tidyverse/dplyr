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
  dots <- tidy_quotes(...)
  arrange_impl(.data, dots)
}
#' @export
arrange_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  arrange_impl(.data, dots)
}

#' @export
filter.tbl_df <- function(.data, ...) {
  dots <- tidy_quotes(...)
  if (any(have_names(dots))) {
    abort("filter() takes unnamed arguments. Do you need `==`?")
  }
  dots <- exprs_auto_name(dots)
  filter_impl(.data, dots)
}
#' @export
filter_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!! dots)
}

#' @export
slice.tbl_df <- function(.data, ...) {
  dots <- tidy_quotes(..., .named = TRUE)
  slice_impl(.data, dots)
}
#' @export
slice_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  slice_impl(.data, dots)
}

#' @export
mutate.tbl_df <- function(.data, ...) {
  dots <- tidy_quotes(..., .named = TRUE)
  mutate_impl(.data, dots)
}
#' @export
mutate_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  mutate_impl(.data, dots)
}

#' @export
summarise.tbl_df <- function(.data, ...) {
  dots <- tidy_quotes(..., .named = TRUE)
  summarise_impl(.data, dots)
}
#' @export
summarise_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise_impl(.data, dots)
}

# Joins ------------------------------------------------------------------------

#' Join data frame tbls.
#'
#' See [join] for a description of the general purpose of the
#' functions.
#'
#' @inheritParams inner_join
#' @param ... included for compatibility with the generic; otherwise ignored.
#' @param na_match Use `FALSE` to always treat two `NA` or `NaN` values as
#'   different, similarly to joins in relational databases or to
#'   `merge(incomparables = FALSE)`. The default `TRUE` treats two `NA` or `NaN`
#'   values as equal, like [merge()].
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

#' @export
#' @rdname join.tbl_df
inner_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = c("na", "never")) {
  na_matches <- match.arg(na_matches)
  accept_na_match <- (na_matches == "na")

  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)

  y <- auto_copy(x, y, copy = copy)

  inner_join_impl(x, y, by$x, by$y, suffix$x, suffix$y, accept_na_match)
}

#' @export
#' @rdname join.tbl_df
left_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = c("na", "never")) {
  na_matches <- match.arg(na_matches)
  accept_na_match <- (na_matches == "na")

  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)

  y <- auto_copy(x, y, copy = copy)

  left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y, accept_na_match)
}

#' @export
#' @rdname join.tbl_df
right_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = c("na", "never")) {
  na_matches <- match.arg(na_matches)
  accept_na_match <- (na_matches == "na")

  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)

  y <- auto_copy(x, y, copy = copy)
  right_join_impl(x, y, by$x, by$y, suffix$x, suffix$y, accept_na_match)
}

#' @export
#' @rdname join.tbl_df
full_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = c("na", "never")) {
  na_matches <- match.arg(na_matches)
  accept_na_match <- (na_matches == "na")

  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)

  y <- auto_copy(x, y, copy = copy)
  full_join_impl(x, y, by$x, by$y, suffix$x, suffix$y, accept_na_match)
}

#' @export
#' @rdname join.tbl_df
semi_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = c("na", "never")) {
  na_matches <- match.arg(na_matches)
  accept_na_match <- (na_matches == "na")

  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  semi_join_impl(x, y, by$x, by$y, accept_na_match)
}

#' @export
#' @rdname join.tbl_df
anti_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = c("na", "never")) {
  na_matches <- match.arg(na_matches)
  accept_na_match <- (na_matches == "na")

  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  anti_join_impl(x, y, by$x, by$y, accept_na_match)
}


# Set operations ---------------------------------------------------------------

#' @export
distinct.tbl_df <- function(.data, ...) {
  tbl_df(NextMethod())
}
#' @export
distinct_.tbl_df <- function(.data, ..., .dots = list()) {
  tbl_df(NextMethod())
}
