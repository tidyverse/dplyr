#' Semi joins and anti joins.
#' 
#' A semi join keeps all records from \code{x} that have matching rows in 
#' \code{y}. It preserves all columns from \code{x}, but none from \code{y}
#' 
#' @inheritParams join
#' @param anti If \code{TRUE}, performs an anti join instead of a semi join.
#' @export
semi_join <- function(x, y, by = NULL, anti = FALSE, copy = FALSE, ...) {
  UseMethod("semi_join")
}

#' Semi-join for SQLite tbls.
#' 
#' @inheritParams join
#' @param anti If \code{TRUE}, performs an anti join instead of a semi join.
#'   Anti joins use \code{WHERE NOT EXISTS} rather than \code{WHERE EXISTS}
#' @inheritParams join.tbl_sqlite
#' @export
#' @examples
#' people <- tbl(lahman(), "Master")
#' 
#' # All people in half of fame
#' hof <- tbl(lahman(), "HallOfFame")
#' semi_join(people, hof)
#' 
#' # All people not in the hall of fame
#' semi_join(people, hof, anti = TRUE)
#' 
#' # Find all managers
#' manager <- tbl(lahman(), "Managers")
#' semi_join(people, manager)
#' 
#' # Find all managers in hall of fame
#' semi_join(semi_join(people, manager), hof)
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
  
  tbl(x$src, from)
}
