#' Connect to a sqlite database.
#'
#' Use \code{src_sqlite} to connect to an existing sqlite database,
#' and \code{tbl} to connect to tables within that database.
#' If you are running a local sqliteql database, leave all parameters set as
#' their defaults to connect. If you're connecting to a remote database,
#' ask your database administrator for the values of these variables.
#' \code{src_memdb} is an easy way to use an in-memory SQLite database
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

  src_sql("sqlite", con, path = path, info = DBI::dbGetInfo(con))
}

#' @export
#' @rdname src_sqlite
src_memdb <- function() {
  cache_computation("src_memdb", src_sqlite(":memory:", TRUE))
}

#' @export
#' @rdname src_sqlite
tbl.src_sqlite <- function(src, from, ...) {
  tbl_sql("sqlite", src = src, from = from, ...)
}

#' @export
src_desc.src_sqlite <- function(x) {
  paste0("sqlite ", x$info$serverVersion, " [", x$path, "]")
}

#' @export
sql_translate_env.SQLiteConnection <- function(x) {
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

# DBI methods ------------------------------------------------------------------

#' @export
db_query_fields.SQLiteConnection <- function(con, sql, ...) {
  rs <- DBI::dbSendQuery(con, build_sql("SELECT * FROM ", sql))
  on.exit(DBI::dbClearResult(rs))

  names(fetch(rs, 0L))
}

#' @export
db_insert_into.SQLiteConnection <- function(con, table, values, ...) {
  DBI::dbWriteTable(con, table, values, append = TRUE, row.names = FALSE)
}
