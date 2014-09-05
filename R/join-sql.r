#' Join sql tbls.
#'
#' See \code{\link{join}} for a description of the general purpose of the
#' functions.
#'
#' @section Implementation notes:
#'
#' Semi-joins are implemented using \code{WHERE EXISTS}, and anti-joins with
#' \code{WHERE NOT EXISTS}. Support for semi-joins is somewhat partial: you
#' can only create semi joins where the \code{x} and \code{y} columns are
#' compared with \code{=} not with more general operators.
#'
#' @inheritParams join
#' @param x,y tbls to join
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into a
#'   temporary table in same database as \code{x}. \code{join} will automatically
#'   run \code{ANALYZE} on the created table in the hope that this will make
#'   you queries as efficient as possible by giving more data to the query
#'   planner.
#'
#'   This allows you to join tables across srcs, but it's potentially expensive
#'   operation so you must opt into it.
#' @param auto_index if \code{copy} is \code{TRUE}, automatically create
#'   indices for the variables in \code{by}. This may speed up the join if
#'   there are matching indexes in \code{x}.
#' @examples
#' \donttest{
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'
#' # Left joins ----------------------------------------------------------------
#' batting <- tbl(lahman_sqlite(), "Batting")
#' team_info <- select(tbl(lahman_sqlite(), "Teams"), yearID, lgID, teamID, G, R:H)
#'
#' # Combine player and whole team statistics
#' first_stint <- select(filter(batting, stint == 1), playerID:H)
#' both <- left_join(first_stint, team_info, type = "inner", by = c("yearID", "teamID", "lgID"))
#' head(both)
#' explain(both)
#'
#' # Join with a local data frame
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"),
#'   yearID = 2010:2012)
#' top4a <- left_join(batting, grid, copy = TRUE)
#' explain(top4a)
#'
#' # Indices don't really help here because there's no matching index on
#' # batting
#' top4b <- left_join(batting, grid, copy = TRUE, auto_index = TRUE)
#' explain(top4b)
#'
#' # Semi-joins ----------------------------------------------------------------
#'
#' people <- tbl(lahman_sqlite(), "Master")
#'
#' # All people in half of fame
#' hof <- tbl(lahman_sqlite(), "HallOfFame")
#' semi_join(people, hof)
#'
#' # All people not in the hall of fame
#' anti_join(people, hof)
#'
#' # Find all managers
#' manager <- tbl(lahman_sqlite(), "Managers")
#' semi_join(people, manager)
#'
#' # Find all managers in hall of fame
#' famous_manager <- semi_join(semi_join(people, manager), hof)
#' famous_manager
#' explain(famous_manager)
#'
#' # Anti-joins ----------------------------------------------------------------
#'
#' # batters without person covariates
#' anti_join(batting, people)
#' }
#' }
#' @name join.tbl_sql
NULL

#' @rdname join.tbl_sql
#' @export
inner_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
                                  auto_index = FALSE, ...) {
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))
  sql <- sql_join(x$src$con, x, y, type = "inner", by = by)
  update(tbl(x$src, sql), group_by = groups(x))
}

#' @rdname join.tbl_sql
#' @export
left_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
                                 auto_index = FALSE, ...) {
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))
  sql <- sql_join(x$src$con, x, y, type = "left", by = by)
  update(tbl(x$src, sql), group_by = groups(x))
}

#' @rdname join.tbl_sql
#' @export
semi_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
                                 auto_index = FALSE, ...) {
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))
  sql <- sql_semi_join(x$src$con, x, y, anti = FALSE, by = by)
  update(tbl(x$src, sql), group_by = groups(x))
}

#' @rdname join.tbl_sql
#' @export
anti_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
                                 auto_index = FALSE, ...) {
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))
  sql <- sql_semi_join(x$src$con, x, y, anti = TRUE, by = by)
  update(tbl(x$src, sql), group_by = groups(x))
}

is.join <- function(x) {
  inherits(x, "join")
}
