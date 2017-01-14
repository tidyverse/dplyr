#' Create a data frame tbl.
#'
#' Forwards the argument to \code{\link[tibble]{as_data_frame}}, see
#' \link{tibble-package} for more details.
#'
#' @export
#' @param data a data frame
#' @examples
#' ds <- tbl_df(mtcars)
#' ds
#' as.data.frame(ds)
#'
#' if (require("Lahman") && packageVersion("Lahman") >= "3.0.1") {
#' batting <- tbl_df(Batting)
#' dim(batting)
#' colnames(batting)
#' head(batting)
#'
#' # Data manipulation verbs ---------------------------------------------------
#' filter(batting, yearID > 2005, G > 130)
#' select(batting, playerID:lgID)
#' arrange(batting, playerID, desc(yearID))
#' summarise(batting, G = mean(G), n = n())
#' mutate(batting, rbi2 = if(is.null(AB)) 1.0 * R / AB else 0)
#'
#' # Group by operations -------------------------------------------------------
#' # To perform operations by group, create a grouped object with group_by
#' players <- group_by(batting, playerID)
#' head(group_size(players), 100)
#'
#' summarise(players, mean_g = mean(G), best_ab = max(AB))
#' best_year <- filter(players, AB == max(AB) | G == max(G))
#' progress <- mutate(players, cyear = yearID - min(yearID) + 1,
#'  rank(desc(AB)), cumsum(AB))
#'
#' # When you group by multiple level, each summarise peels off one level
#' \donttest{
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(stints, stints > 3)
#' summarise(stints, max(stints))
#' mutate(stints, cumsum(stints))
#' }
#'
#' # Joins ---------------------------------------------------------------------
#' player_info <- select(tbl_df(Master), playerID, birthYear)
#' hof <- select(filter(tbl_df(HallOfFame), inducted == "Y"),
#'  playerID, votedBy, category)
#'
#' # Match players and their hall of fame data
#' inner_join(player_info, hof)
#' # Keep all players, match hof data where available
#' left_join(player_info, hof)
#' # Find only players in hof
#' semi_join(player_info, hof)
#' # Find players not in hof
#' anti_join(player_info, hof)
#' }
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
