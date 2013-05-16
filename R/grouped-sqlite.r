#' A grouped sqlite database.
#'
#' Typically you will create a grouped data table is to call the \code{group_by}
#' method on a sqlite data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @section Performance:
#'
#' For best performance, the database should have an index on the variables
#' that you are grouping by. A good introduction to how indices affect database
#' performance can be found at \url{http://www.sqlite.org/queryplanner.html}.
#'
#' @param source a data source or data frame.
#' @param vars a list of quoted variables.
#' @param group_by \code{vars} partially evaluated in the correct environment
#' @export
#' @examples
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball_s <- source_sqlite(db_path, "baseball")
#'
#' by_year_lg <- group_by(baseball_s, year, lg)
#' summarise(by_year_lg, players = count(), avg_g = mean(g))
#'
#' by_team <- group_by(baseball_s, team)
#' summarise(by_team, players = count())
grouped_sqlite <- function(source, vars, group_by) {
  source$vars <- vars
  source$group_by <- group_by

  structure(source, class = c("grouped_sqlite", "source_sqlite", "source"))
}

#' @export
#' @rdname grouped_sqlite
#' @method group_by source_sqlite
#' @param x an existing sqlite data source
#' @param ... expressions describing how to group data
group_by.source_sqlite <- function(x, ...) {
  vars <- named_dots(...)
  group_by <- partial_eval(x, vars, parent.frame())

  grouped_sqlite(x, vars, group_by)
}

#' @S3method print grouped_sqlite
print.grouped_sqlite <- function(x, ...) {
  cat("Source: SQLite [", x$path, "]\n", sep = "")
  cat("Table:  ", x$table, dim_desc(x), "\n", sep = "")
  cat("Groups: ", paste0(deparse_all(x$vars), collapse = ", "), "\n", sep = "")

  cat("\n")
  trunc_mat(x)
}
