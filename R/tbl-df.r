#' @importFrom tibble tbl_df
#' @export
tibble::tbl_df

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
arrange_.tbl_df  <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  arrange_impl(.data, dots)
}

#' @export
filter_.tbl_df    <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  if (any(has_names(dots))) {
    stop("filter() takes unnamed arguments. Do you need `==`?", call. = FALSE)
  }
  # C++ code assumes that elements are named, so give them automatic names
  dots <- lazyeval::auto_name(dots)

  filter_impl(.data, dots)
}

#' @export
slice_.tbl_df  <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  slice_impl(.data, dots)
}

#' @export
mutate_.tbl_df  <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  mutate_impl(.data, dots)
}

#' @export
summarise_.tbl_df <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  summarise_impl(.data, dots)
}

# Joins ------------------------------------------------------------------------

#' Join data frame tbls.
#'
#' See \code{\link{join}} for a description of the general purpose of the
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


# Other methods that currently don't have a better home -----------------------

order_ <- function(..., data){
  parent_frame <- parent.frame()
  if(missing(data)) {
    env <- parent_frame
  } else {
    env <- as.environment(data)
    parent.env(env) <- parent_frame
  }
  order_impl(dots(...) , env)
}

equal_ <- function(x, y){
  equal_data_frame(x, y)
}

all_equal_ <- function(...){
  env <- parent.frame()
  all_equal_data_frame(dots(...), env)
}

sort_ <- function(data){
  sort_impl(data)
}
