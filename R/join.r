#' Join two tbls together
#' 
#' Groups are ignored for the purpose of joining, but the result preserves
#' the grouping of \code{x}.
#' 
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If \code{NULL}, the
#'   default, \code{join} will do a natural join, using all variables with 
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right. 
#' @param type a string giving the join type. Possible values are left (the 
#'   default), right, full and inner. Not all types will be supported by all
#'   tbls.
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into the 
#'   same src as \code{x}.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param ... other parameters passed onto individual methods
#' @export
join <- function(x, y, by = NULL, type = "left", copy = FALSE, ...) {
  UseMethod("join")
}

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

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

from <- function(x) {
  if (is_table(x)) {
    x$table
  } else {
    build_sql("(", x$query$sql, ")")
  }  
}

common_by <- function(x, y) {
  by <- intersect(tbl_vars(x), tbl_vars(y))
  message("Joining by: ", capture.output(dput(by)))
  by
}