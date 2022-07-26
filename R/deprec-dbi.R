#' Source for database backends
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions are defunct; instead loading dbplyr and calling [tbl()]
#' directly on an `DBIConnection`. See <https://dbplyr.tidyverse.org/> for
#' more details.
#'
#' @param dbname Database name
#' @param host,port Host name and port number of database
#' @param user,username,password User name and password.
#'
#'   Generally, you should avoid saving username and password in your
#'   scripts as it is easy to accidentally expose valuable credentials.
#'   Instead, retrieve them from environment variables, or use database
#'   specific credential scores. For example, with MySQL you can set up `my.cnf`
#'   as described in [RMySQL::MySQL()].
#' @param ... for the src, other arguments passed on to the underlying
#'   database connector, [DBI::dbConnect()]. For the tbl, included for
#'   compatibility with the generic, but otherwise ignored.
#' @return An S3 object with class `src_dbi`, `src_sql`, `src`.
#' @keywords internal
#' @name src_dbi
NULL

#' @rdname src_dbi
#' @export
src_mysql <- function(dbname, host = NULL, port = 0L, username = "root",
                      password = "", ...) {
  lifecycle::deprecate_stop(
    "1.0.0", "dplyr::src_mysql()",
    details = "Please load dbplyr and use `tbl()` directly with a database connection"
  )
}

#' @rdname src_dbi
#' @export
src_postgres <- function(dbname = NULL, host = NULL, port = NULL,
                         user = NULL, password = NULL, ...) {
  lifecycle::deprecate_stop(
    "1.0.0", "dplyr::src_postgres()",
    details = "Please load dbplyr and use `tbl()` directly with a database connection"
  )
}

#' @rdname src_dbi
#' @export
#' @param path Path to SQLite database. You can use the special path
#'   ":memory:" to create a temporary in memory database.
#' @param create if `FALSE`, `path` must already exist. If
#'   `TRUE`, will create a new SQLite3 database at `path` if
#'   `path` does not exist and connect to the existing database if
#'   `path` does exist.
src_sqlite <- function(path, create = FALSE) {
  lifecycle::deprecate_stop(
    "1.0.0", "dplyr::src_sqlite()",
    details = "Please load dbplyr and use `tbl()` directly with a database connection"
  )
}
