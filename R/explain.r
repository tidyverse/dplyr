#' Explain details of a tbl
#'
#' This is a generic function which gives more details about an object than
#' [print()], and is more focussed on human readable output than
#' [str()].
#'
#' @section Databases:
#' Explaining a `tbl_sql` will run the SQL `EXPLAIN` command which
#' will describe the query plan. This requires a little bit of knowledge about
#' how `EXPLAIN` works for your database, but is very useful for
#' diagnosing performance problems.
#'
#' @export
#' @param x An object to explain
#' @param ... Other parameters possibly used by generic
#' @return The first argument, invisibly.
#' @examples
#' \donttest{
#' if (require("dbplyr")) {
#'
#' lahman_s <- lahman_sqlite()
#' batting <- tbl(lahman_s, "Batting")
#' batting %>% show_query()
#' batting %>% explain()
#'
#' # The batting database has indices on all ID variables:
#' # SQLite automatically picks the most restrictive index
#' batting %>% filter(lgID == "NL" & yearID == 2000L) %>% explain()
#'
#' # OR's will use multiple indexes
#' batting %>% filter(lgID == "NL" | yearID == 2000) %>% explain()
#'
#' # Joins will use indexes in both tables
#' teams <- tbl(lahman_s, "Teams")
#' batting %>% left_join(teams, c("yearID", "teamID")) %>% explain()
#' }
#' }
explain <- function(x, ...) {
  UseMethod("explain")
}

#' @export
#' @rdname explain
show_query <- function(x, ...) {
  UseMethod("show_query")
}
