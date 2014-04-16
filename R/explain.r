#' Show sql and query plans.
#'
#' Any queries run inside this function will automatically be explained:
#' displaying information about which indexes are used to optimise the query.
#' This requires a little bit of knowledge about how \code{EXPLAIN} works for
#' your database, but is very useful for diagnosing performance problems.
#'
#' @param code code to run. All sql queries executed during the running of the
#'   code will be shown and explained.
#' @param x an sql-based table to explain.
#' @param ... Ignored. Needed for compatibility with generic.
#' @export
#' @examples
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'
#' batting <- tbl(lahman_sqlite(), "Batting")
#'
#' # Note that you have to do something that actually triggers a query
#' # inside the explain function
#' explain_sql(nrow(batting))
#' explain_sql(nrow(batting))
#'
#' # nrow requires two queries the first time because it's the same as dim(x)[1]
#' # but the results are cached
#'
#' show_sql(head(batting))
#' explain_sql(head(batting))
#'
#' # If you just want to understand the sql for a tbl, use explain
#' explain(batting)
#'
#' # The batting database has indices on all ID variables:
#' # SQLite automatically picks the most restrictive index
#' explain(filter(batting, lgID == "NL" & yearID == 2000))
#'
#' # OR's will use multiple indexes
#' explain(filter(batting, lgID == "NL" | yearID == 2000))
#' }
explain_sql <- function(code) {
  old <- options(dplyr.explain_sql = TRUE, dplyr.show_sql = TRUE)
  on.exit(options(old))

  code
}

#' @export
#' @rdname explain_sql
show_sql <- function(code) {
  old <- options(dplyr.show_sql = TRUE)
  on.exit(options(old))

  code
}

#' @export
#' @rdname explain_sql
explain.tbl_sql <- function(x, ...) {
  force(x)
  message(
    "<SQL>\n", x$query$sql,
    "\n\n",
    "<PLAN>\n", qry_explain(x$query$con, x$query$sql)
  )

  invisible(NULL)
}

#' Explain details of an object
#'
#' This is a generic function which gives more details about an object than
#' \code{\link{print}}, and is more focussed on human readable output than
#' \code{\link{str}}.
#'
#' For more details of explaining dplyr sql tbls, see \code{\link{explain_sql}}.
#'
#' @export
#' @param x An object to explain
#' @param ... Other parameters possibly used by generic
explain <- function(x, ...) {
  UseMethod("explain")
}
