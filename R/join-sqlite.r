#' Join SQLite tbls.
#' 
#' @inheritParams join
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into a 
#'   temporary table in same database as \code{x}. \code{join} will automatically 
#'   run \code{ANALYSE} on the created table in the hope that this will make
#'   you queries as efficient as possible by giving more data to the query
#'   planner.
#'   
#'   This allows you to join tables across srcs, but it's potentially expensive 
#'   operation so you must opt into it.
#' @param auto_index if \code{copy} is \code{TRUE}, automatically create 
#'   indices for the variables in \code{by}. This may speed up the join if
#'   there are matching indices in \code{x}.
#' @method join tbl_sqlite
#' @export
#' @examples
#' batting <- tbl(src_lahman(), "Batting")
#' team_info <- select(tbl(src_lahman(), "Teams"), yearID, lgID, teamID, G, R:H)
#'
#' # Combine player and whole team statistics
#' first_stint <- select(filter(batting, stint == 1), playerID:H)
#' both <- join(first_stint, team_info, type = "inner", by = c("yearID", "teamID", "lgID"))
#' head(both)
#' explain_tbl(both)
#' 
#' # Join with a local data frame
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"), 
#'   yearID = 2010:2012)
#' top4a <- join(batting, grid, copy = TRUE, type = "inner")
#' explain_tbl(top4a)
#' 
#' # Indices don't really help here because there's no matching index on
#' # batting
#' top4b <- join(batting, grid, copy = TRUE, type = "inner", auto_index = TRUE)
#' explain_tbl(top4b)
join.tbl_sqlite <- function(x, y, by = NULL, type = "left", copy = FALSE, 
  auto_index = FALSE, ...) {
  type <- match.arg(type, c("left", "right", "inner", "full"))
  by <- by %||% common_by(x, y)
  
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))
  
  join <- switch(type, left = sql("LEFT"), inner = sql("INNER"),
    right = stop("Right join not supported", call. = FALSE),
    full = stop("Full join not supported", call. = FALSE))
  
  from <- build_sql(from(x), "\n\n", 
    join, " JOIN \n\n" , 
    from(y), "\n\n",
    "USING ", lapply(by, ident))
  
  update(tbl(x$src, from), group_by = groups(x))
}

#' Semi-join for SQLite tbls.
#' 
#' Semi-joins are implemented using \code{WHERE EXISTS}, and anti-joins with
#' \code{WHERE NOT EXISTS}. Support for semi-joins is somewhat partial: you 
#' can only create semi joins where the \code{x} and \code{y} columns are
#' compared with \code{=} not with more general operators.
#' 
#' @inheritParams join
#' @inheritParams join.tbl_sqlite
#' @param anti If \code{TRUE}, performs an anti join instead of a semi join.
#'   Anti joins use \code{WHERE NOT EXISTS} rather than \code{WHERE EXISTS}
#' @export
#' @examples
#' people <- tbl(src_lahman(), "Master")
#' 
#' # All people in half of fame
#' hof <- tbl(src_lahman(), "HallOfFame")
#' semi_join(people, hof)
#' 
#' # All people not in the hall of fame
#' semi_join(people, hof, anti = TRUE)
#' 
#' # Find all managers
#' manager <- tbl(src_lahman(), "Managers")
#' semi_join(people, manager)
#' 
#' # Find all managers in hall of fame
#' famous_manager <- semi_join(semi_join(people, manager), hof)
#' famous_manager
#' explain_tbl(famous_manager)
semi_join.tbl_sqlite <- function(x, y, by = NULL, anti = FALSE, copy = FALSE, 
  auto_index = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))
  
  by_escaped <- escape(ident(by), collapse = NULL)
  join <- sql(paste0('"_LEFT".', by_escaped, ' = "_RIGHT".', by_escaped, 
    collapse = ' AND '))
  
  from <- build_sql('(',
    'SELECT * FROM ', from(x), ' as "_LEFT"\n\n', 
    'WHERE ', if (anti) sql('NOT '), 'EXISTS (\n',
    '  SELECT 1 FROM ', from(y), ' AS "_RIGHT"\n',
    '  WHERE ', join, ')',
    ')')
  
  update(tbl(x$src, from), group_by = groups(x))
}

from <- function(x) {
  if (is_table(x)) {
    x$table
  } else {
    build_sql("(", x$query$sql, ")")
  }  
}
