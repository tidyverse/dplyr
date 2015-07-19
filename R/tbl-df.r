#' Create a data frame tbl.
#'
#' A data frame tbl wraps a local data frame. The main advantage to using
#' a \code{tbl_df} over a regular data frame is the printing:
#' tbl objects only print a few rows and all the columns that fit on one
#' screen, describing the rest of it as text.
#'
#' @section Methods:
#'
#' \code{tbl_df} implements two important base methods:
#'
#' \describe{
#' \item{print}{Only prints the first 10 rows, and the columns that fit on
#'   screen}
#' \item{\code{[}}{Never simplifies (drops), so always returns data.frame}
#' }
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
  assert_that(is.data.frame(data))
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

#' @rdname dplyr-formatting
#' @export
<<<<<<< HEAD
print.tbl_df <- function(x, ..., n = NULL, m = NULL, width = NULL, show_classes = NULL) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("\n")
  print(trunc_mat(x, n = n, m = m, width = width, show_classes = show_classes))
=======
print.tbl_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("\n")
  print(trunc_mat(x, n = n, width = width))
>>>>>>> upstream/master

  invisible(x)
}

.check_names_df <- function(x, j){
  if( is.character(j) && any( wrong <- ! j %in% names(x) ) ){
    names <- j[wrong]
    stop( sprintf( "undefined columns: %s", paste(names, collapse = ", " ) ) ) ;
  }
}

#' @export
`[.tbl_df` <- function (x, i, j, drop = FALSE) {
  if (missing(i) && missing(j)) return(x)
  if (drop) warning("drop ignored", call. = FALSE)

  nr <- nrow(x)

  # Escape early if nargs() == 2L; ie, column subsetting
  if (nargs() == 2L) {
    .check_names_df(x,i)
    result <- .subset(x, i)
    class(result) <- c("tbl_df", "data.frame")
    attr(result, "row.names") <- .set_row_names(nr)
    return(result)
  }

  # First, subset columns
  if (!missing(j)) {
    .check_names_df(x,j)
    x <- .subset(x, j)
  }

  # Next, subset rows
  if (!missing(i)) {
    if (length(x) == 0) {
      nr <- length(attr(x, "row.names")[i])
    } else {
      x <- lapply(x, `[`, i)
      nr <- length(x[[1]])
    }
  }

  class(x) <- c("tbl_df", "data.frame")
  attr(x, "row.names") <- .set_row_names(nr)
  x
}


# Verbs ------------------------------------------------------------------------

#' @export
arrange_.tbl_df  <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  arrange_impl(.data, dots)
}

#' @export
filter_.tbl_df    <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
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
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If \code{NULL}, the
#'   default, \code{join} will do a natural join, using all variables with
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right - to suppress the message, supply
#'   a character vector.
#' @param copy If \code{y} is not a data frame or \code{\link{tbl_df}} and
#'   \code{copy} is \code{TRUE}, \code{y} will be converted into a data frame
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
inner_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)

  inner_join_impl(x, y, by$x, by$y)
}

#' @export
#' @rdname join.tbl_df
left_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  left_join_impl(x, y, by$x, by$y)
}

#' @export
#' @rdname join.tbl_df
right_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  right_join_impl(x, y, by$x, by$y)
}

#' @export
#' @rdname join.tbl_df
full_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  outer_join_impl(x, y, by$x, by$y)
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
