#' Source for database backends
#'
#' @description
#' For backward compatibility dplyr provides three srcs for popular
#' open source databases:
#'
#' * `src_mysql()` connects to a MySQL or MariaDB database using [RMySQL::MySQL()].
#' * `src_postgres()` connects to PostgreSQL using [RPostgreSQL::PostgreSQL()]
#' * `src_sqlite()` to connect to a SQLite database using [RSQLite::SQLite()].
#'
#' However, modern best practice is to use [tbl()] directly on an `DBIConnection`.
#'
#' @details
#' All data manipulation on SQL tbls are lazy: they will not actually
#' run the query or retrieve the data unless you ask for it: they all return
#' a new `tbl_dbi` object. Use [compute()] to run the query and save the
#' results in a temporary in the database, or use [collect()] to retrieve the
#' results to R. You can see the query with [show_query()].
#'
#' For best performance, the database should have an index on the variables
#' that you are grouping by. Use [explain()] to check that the database is using
#' the indexes that you expect.
#'
#' There is one exception: [do()] is not lazy since it must pull the data
#' into R.
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
#' @examples
#' # Basic connection using DBI -------------------------------------------
#' if (require(dbplyr, quietly = TRUE)) {
#'
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' copy_to(con, mtcars)
#'
#' DBI::dbListTables(con)
#'
#' # To retrieve a single table from a source, use `tbl()`
#' con %>% tbl("mtcars")
#'
#' # You can also use pass raw SQL if you want a more sophisticated query
#' con %>% tbl(sql("SELECT * FROM mtcars WHERE cyl == 8"))
#'
#' # To show off the full features of dplyr's database integration,
#' # we'll use the Lahman database. lahman_sqlite() takes care of
#' # creating the database.
#' lahman_p <- lahman_sqlite()
#' batting <- lahman_p %>% tbl("Batting")
#' batting
#'
#' # Basic data manipulation verbs work in the same way as with a tibble
#' batting %>% filter(yearID > 2005, G > 130)
#' batting %>% select(playerID:lgID)
#' batting %>% arrange(playerID, desc(yearID))
#' batting %>% summarise(G = mean(G), n = n())
#'
#' # There are a few exceptions. For example, databases give integer results
#' # when dividing one integer by another. Multiply by 1 to fix the problem
#' batting %>%
#'   select(playerID:lgID, AB, R, G) %>%
#'   mutate(
#'    R_per_game1 = R / G,
#'    R_per_game2 = R * 1.0 / G
#'  )
#'
#' # All operations are lazy: they don't do anything until you request the
#' # data, either by `print()`ing it (which shows the first ten rows),
#' # or by `collect()`ing the results locally.
#' system.time(recent <- filter(batting, yearID > 2010))
#' system.time(collect(recent))
#'
#' # You can see the query that dplyr creates with show_query()
#' batting %>%
#'   filter(G > 0) %>%
#'   group_by(playerID) %>%
#'   summarise(n = n()) %>%
#'   show_query()
#' }
#' @name src_dbi
NULL

#' @rdname src_dbi
#' @export
src_mysql <- function(dbname, host = NULL, port = 0L, username = "root",
                      password = "", ...) {
  check_dbplyr()
  check_pkg("RMySQL", "connect to MySQL/MariaDB")

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

  if (!create && !file.exists(path)) {
    bad_args("path", "must already exist, unless `create` = TRUE")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  RSQLite::initExtension(con)

  dbplyr::src_dbi(con, auto_disconnect = TRUE)
}

# S3 methods --------------------------------------------------------------

#' @export
tbl.DBIConnection <- function(src, from, ...) {
  check_dbplyr()
  tbl(dbplyr::src_dbi(src, auto_disconnect = FALSE), from = from, ...)
}

#' @export
copy_to.DBIConnection <- function(dest, df, name = deparse(substitute(df)),
                                  overwrite = FALSE, ...) {
  check_dbplyr()
  copy_to(
    dbplyr::src_dbi(dest, auto_disconnect = FALSE),
    df = df,
    name = name,
    overwrite = overwrite,
    ...
  )
}
