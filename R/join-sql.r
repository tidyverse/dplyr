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
#' @param by a character vector of variables to join by.  If \code{NULL}, the
#'   default, \code{join} will do a natural join, using all variables with
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right - to suppress the message, supply
#'   a character vector.
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
#' explain_tbl(both)
#'
#' # Join with a local data frame
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"),
#'   yearID = 2010:2012)
#' top4a <- left_join(batting, grid, copy = TRUE)
#' explain_tbl(top4a)
#'
#' # Indices don't really help here because there's no matching index on
#' # batting
#' top4b <- left_join(batting, grid, copy = TRUE, auto_index = TRUE)
#' explain_tbl(top4b)
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
#' semi_join(people, hof, anti = TRUE)
#'
#' # Find all managers
#' manager <- tbl(lahman_sqlite(), "Managers")
#' semi_join(people, manager)
#'
#' # Find all managers in hall of fame
#' famous_manager <- semi_join(semi_join(people, manager), hof)
#' famous_manager
#' explain_tbl(famous_manager)
#'
#' # Anti-joins ----------------------------------------------------------------
#'
#' # batters without person covariates
#' anti_join(batting, people)
#' }
#' @name join.tbl_sql
NULL

#' @rdname join.tbl_sql
#' @method inner_join tbl_sql
#' @export
inner_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
                                  auto_index = FALSE, ...) {
  join_sql(x, y, "inner", by = by, copy = copy, auto_index = auto_index, ...)
}

#' @rdname join.tbl_sql
#' @method left_join tbl_sql
#' @export
left_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
                                 auto_index = FALSE, ...) {
  join_sql(x, y, "left", by = by, copy = copy, auto_index = auto_index, ...)
}


#' @rdname join.tbl_sql
#' @method semi_join tbl_sql
#' @export
semi_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
                                 auto_index = FALSE, ...) {
  semi_join_sql(x, y, FALSE, by = by, copy = copy, auto_index = auto_index,
    ...)
}

#' @rdname join.tbl_sql
#' @method anti_join tbl_sql
#' @export
anti_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
                                 auto_index = FALSE, ...) {
  semi_join_sql(x, y, TRUE, by = by, copy = copy, auto_index = auto_index,
    ...)
}

join_sql <- function(x, y, type, by = NULL, copy = FALSE, auto_index = FALSE,
  ...) {
  type <- match.arg(type, c("left", "right", "inner", "full"))
  by <- by %||% common_by(x, y)
  
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))

  # Ensure tables have unique names
  x_names <- auto_names(x$select)
  y_names <- auto_names(y$select) 
  common <- setdiff(intersect(x_names, y_names), by)
  if (length(common) > 0) {
    x_match <- match(common, x_names)    
    x_names[x_match] <- paste0(x_names[x_match], ".x")
    
    y_match <- match(common, y_names)    
    y_names[y_match] <- paste0(y_names[y_match], ".y")
    
    x <- update(x, select = setNames(x$select, x_names))
    y <- update(x, select = setNames(y$select, y_names))
  }
  vars <- lapply(c(by, setdiff(c(x_names, y_names), by)), as.name)
  
  join <- switch(type, left = sql("LEFT"), inner = sql("INNER"),
    right = stop("Right join not supported", call. = FALSE),
    full = stop("Full join not supported", call. = FALSE))

  from <- build_sql(from(x), "\n\n",
    join, " JOIN \n\n" ,
    from(y), "\n\n",
    "USING ", lapply(by, ident), con = x$src$con)

  update(tbl(x$src, as.join(from), vars = vars), group_by = groups(x))
}

as.join <- function(x) {
  structure(x, class = c("join", class(x)))
}
is.join <- function(x) {
  inherits(x, "join")
}

semi_join_sql <- function(x, y, anti = FALSE, by = NULL, copy = FALSE,
  auto_index = FALSE, ...) {
  
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))

  con <- x$src$con
  by_escaped <- escape(ident(by), collapse = NULL, con = con)
  left <- escape(ident("_LEFT"), con = con)
  right <- escape(ident("_RIGHT"), con = con)
  
  join <- sql(paste0(left, ".", by_escaped, " = ", right, ".", by_escaped, 
    collapse = " AND "))

  from <- build_sql(
    'SELECT * FROM ', from(x, "_LEFT"), '\n\n',
    'WHERE ', if (anti) sql('NOT '), 'EXISTS (\n',
    '  SELECT 1 FROM ', from(y, "_RIGHT"), '\n',
    '  WHERE ', join, ')'
  )

  update(tbl(x$src, from, vars = x$select), group_by = groups(x))
}

from <- function(x, name = random_table_name()) {
  build_sql("(", x$query$sql, ") AS ", ident(name), con = x$src$con)
}
