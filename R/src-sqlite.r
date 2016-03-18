#' Connect to a sqlite database.
#'
#' Use \code{src_sqlite} to connect to an existing sqlite database,
#' and \code{tbl} to connect to tables within that database.
#' If you are running a local sqliteql database, leave all parameters set as
#' their defaults to connect. If you're connecting to a remote database,
#' ask your database administrator for the values of these variables.
#' \code{\link{src_memdb}} is an easy way to use an in-memory SQLite database
#' that is scoped to the current session.
#'
#' @template db-info
#' @param path Path to SQLite database
#' @param create if \code{FALSE}, \code{path} must already exist. If
#'   \code{TRUE}, will create a new SQlite3 database at \code{path} if
#'   \code{path} does not exist and connect to the existing database if
#'   \code{path} does exist.
#' @param src a sqlite src created with \code{src_sqlite}.
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @param ... Included for compatibility with the generic, but otherwise
#'   ignored.
#' @export
#' @examples
#' \dontrun{
#' # Connection basics ---------------------------------------------------------
#' # To connect to a database first create a src:
#' my_db <- src_sqlite(path = tempfile(), create = TRUE)
#' # Then reference a tbl within that src
#' my_tbl <- tbl(my_db, "my_table")
#' }
#'
#' # Here we'll use the Lahman database: to create your own local copy,
#' # run lahman_sqlite()
#'
#' \dontrun{
#' if (requireNamespace("RSQLite") && has_lahman("sqlite")) {
#' lahman_s <- lahman_sqlite()
#' # Methods -------------------------------------------------------------------
#' batting <- tbl(lahman_s, "Batting")
#' dim(batting)
#' colnames(batting)
#' head(batting)
#'
#' # Data manipulation verbs ---------------------------------------------------
#' filter(batting, yearID > 2005, G > 130)
#' select(batting, playerID:lgID)
#' arrange(batting, playerID, desc(yearID))
#' summarise(batting, G = mean(G), n = n())
#' mutate(batting, rbi2 = 1.0 * R / AB)
#'
#' # note that all operations are lazy: they don't do anything until you
#' # request the data, either by `print()`ing it (which shows the first ten
#' # rows), by looking at the `head()`, or `collect()` the results locally.
#'
#' system.time(recent <- filter(batting, yearID > 2010))
#' system.time(collect(recent))
#'
#' # Group by operations -------------------------------------------------------
#' # To perform operations by group, create a grouped object with group_by
#' players <- group_by(batting, playerID)
#' group_size(players)
#'
#' # sqlite doesn't support windowed functions, which means that only
#' # grouped summaries are really useful:
#' summarise(players, mean_g = mean(G), best_ab = max(AB))
#'
#' # When you group by multiple level, each summarise peels off one level
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(ungroup(stints), stints > 3)
#' summarise(stints, max(stints))
#'
#' # Joins ---------------------------------------------------------------------
#' player_info <- select(tbl(lahman_s, "Master"), playerID, birthYear)
#' hof <- select(filter(tbl(lahman_s, "HallOfFame"), inducted == "Y"),
#'  playerID, votedBy, category)
#'
#' # Match players and their hall of fame data
#' inner_join(player_info, hof)
#' # Keep all players, match hof data where available
#' left_join(player_info, hof)
#' # Find only players in hof
#' semi_join(player_info, hof)
#' # Find players not in hof
#' anti_join(player_info, hof)
#'
#' # Arbitrary SQL -------------------------------------------------------------
#' # You can also provide sql as is, using the sql function:
#' batting2008 <- tbl(lahman_s,
#'   sql("SELECT * FROM Batting WHERE YearID = 2008"))
#' batting2008
#' }
#' }
src_sqlite <- function(path, create = FALSE) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }

  if (!create && !file.exists(path)) {
    stop("Path does not exist and create = FALSE", call. = FALSE)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  RSQLite::initExtension(con)

  src_sql("sqlite", con, path = path)
}

#' Per-session in-memory SQLite databases.
#'
#' \code{src_memdb} lets you easily access a sessio-temporary in-memory
#' SQLite database. \code{memdb_frame()} works like \code{\link{data_frame}},
#' but instead of creating a new data frame in R, it creates a table in
#' \code{src_memdb}
#'
#' @export
#' @examples
#' if (require("RSQLite")) {
#' src_memdb()
#'
#' df <- memdb_frame(x = runif(100), y = runif(100))
#' df %>% arrange(x)
#' df %>% arrange(x) %>% show_query()
#' }
src_memdb <- function() {
  cache_computation("src_memdb", src_sqlite(":memory:", TRUE))
}

#' @inheritParams data_frame
#' @param .name Name of table in database: defaults to a random name that's
#'   unlikely to conflict with exist
#' @export
#' @rdname src_memdb
memdb_frame <- function(..., .name = random_table_name()) {
  copy_to(src_memdb(), data_frame(...), name = .name)
}

#' @export
#' @rdname src_sqlite
tbl.src_sqlite <- function(src, from, ...) {
  tbl_sql("sqlite", src = src, from = from, ...)
}

#' @export
src_desc.src_sqlite <- function(x) {
  paste0("sqlite ", sqlite_version(), " [", x$path, "]")
}

sqlite_version <- function() {
  if (packageVersion("RSQLite") > 1) {
    RSQLite::rsqliteVersion()[[2]]
  } else {
    DBI::dbGetInfo(RSQLite::SQLite())$clientVersion
  }
}

#' @export
sql_translate_env.SQLiteConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      log = sql_prefix("log")
    ),
    sql_translator(.parent = base_agg,
      sd = sql_prefix("stdev")
    ),
    base_no_win
  )
}

#' @export
sql_escape_ident.SQLiteConnection <- function(con, x) {
  sql_quote(x, '`')
}


# DBI methods ------------------------------------------------------------------

#' @export
db_insert_into.SQLiteConnection <- function(con, table, values, ...) {
  DBI::dbWriteTable(con, table, values, append = TRUE, row.names = FALSE)
}

