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
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  arrange_impl(.data, dots)
}

#' @export
filter_.tbl_df <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  if (any(has_names(dots))) {
    stop("filter() takes unnamed arguments. Do you need `==`?", call. = FALSE)
  }
  # C++ code assumes that elements are named, so give them automatic names
  dots <- lazyeval::auto_name(dots)

  filter_impl(.data, dots)
}

#' @export
slice_.tbl_df <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  slice_impl(.data, dots)
}

#' @export
mutate.tbl_df <- function(.data, ...) {
  dots <- tidy_quotes(..., .named = TRUE)
  mutate_impl(.data, dots)
}
#' @export
mutate_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  mutate_impl(.data, dots)
}

#' @export
summarise.tbl_df <- function(.data, ...) {
  dots <- tidy_quotes(..., .named = TRUE)
  summarise_impl(.data, dots)
}
#' @export
summarise_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
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
                              suffix = c(".x", ".y"), ...) {
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)

  y <- auto_copy(x, y, copy = copy)

  inner_join_impl(x, y, by$x, by$y, suffix$x, suffix$y)
}

#' @export
#' @rdname join.tbl_df
left_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)

  y <- auto_copy(x, y, copy = copy)

  left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y)
}

#' @export
#' @rdname join.tbl_df
right_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)

  y <- auto_copy(x, y, copy = copy)
  right_join_impl(x, y, by$x, by$y, suffix$x, suffix$y)
}

#' @export
#' @rdname join.tbl_df
full_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)

  y <- auto_copy(x, y, copy = copy)
  full_join_impl(x, y, by$x, by$y, suffix$x, suffix$y)
}

#' @export
#' @rdname join.tbl_df
semi_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  semi_join_impl(x, y, by$x, by$y)
}

#' @export
#' @rdname join.tbl_df
anti_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  anti_join_impl(x, y, by$x, by$y)
}


# Set operations ---------------------------------------------------------------

#' @export
distinct_.tbl_df <- function(.data, ..., .dots) {
  tbl_df(NextMethod())
}
