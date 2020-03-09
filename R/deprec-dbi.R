#' Source for database backends
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' These functions have been deprecated; instead please use [tbl()]
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
#' @examples
#' if (require(dbplyr, quietly = TRUE)) {
#'
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' copy_to(con, mtcars)
#'
#' # To retrieve a single table from a source, use `tbl()`
#' mtcars <- con %>% tbl("mtcars")
#' mtcars
#'
#' # You can also use pass raw SQL if you want a more sophisticated query
#' con %>% tbl(sql("SELECT * FROM mtcars WHERE cyl == 8"))
#' }
#' @name src_dbi
NULL

#' @rdname src_dbi
#' @export
src_mysql <- function(dbname, host = NULL, port = 0L, username = "root",
                      password = "", ...) {
  check_dbplyr()
  check_pkg("RMySQL", "connect to MySQL/MariaDB")
  lifecycle::deprecate_warn(
    "1.0.0", "dplyr::src_sqlite()",
    details = "Please use `tbl()` directly with a database connection"
  )

  con <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = dbname,
    host = host,
    port = port,
    username = username,
    password = password,
    ...
  )
  dbplyr::src_dbi(con, auto_disconnect = TRUE)
}

#' @rdname src_dbi
#' @export
src_postgres <- function(dbname = NULL, host = NULL, port = NULL,
                         user = NULL, password = NULL, ...) {
  check_dbplyr()
  check_pkg("RPostgreSQL", "connect to PostgreSQL")
  lifecycle::deprecate_warn(
    "1.0.0", "dplyr::src_sqlite()",
    details = "Please use `tbl()` directly with a database connection"
  )

  user <- user %||% if (in_travis()) "postgres" else ""

  con <- DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = host %||% "",
    dbname = dbname %||% "",
    user = user,
    password = password %||% "",
    port = port %||% "",
    ...
  )

  dbplyr::src_dbi(con, auto_disconnect = TRUE)
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
  check_dbplyr()
  lifecycle::deprecate_warn(
    "1.0.0", "dplyr::src_sqlite()",
    details = "Please use `tbl()` directly with a database connection"
  )

  if (!create && !file.exists(path)) {
    bad_args("path", "must already exist, unless `create` = TRUE")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  RSQLite::initExtension(con)

  dbplyr::src_dbi(con, auto_disconnect = TRUE)
}
